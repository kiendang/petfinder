x <- c(
  "icenReg",
  "tidyverse",
  "irr"
)
x <- lapply(x, require, character.only = TRUE, quietly = T,  warn.conflicts = F)

## load data
setwd("") ## director
df <- read_csv("./train/train.csv")
state_labels <- read_csv("./state_labels.csv")
breed_labels <- read_csv("./breed_labels.csv")
color_labels <- read_csv("./color_labels.csv")


## breed: what are the vocabulary 
not_pure <- c("Domestic Short Hair", "Domestic Medium Hair", "Domestic Long Hair", "Mixed Breed")
  
## named?
not_named <- c("no name|noname|not named|unknown|knoe|found|not yet|^none$|^none $|^non$|not given|no more")
  # train$Name[grepl(not_names, train$Name, ignore.case = TRUE)] # dont over kill
  # nn <- train$Name[!grepl(not_names, train$Name, ignore.case = TRUE)]
  # nn[grepl("no", nn, ignore.case = TRUE)] # check for no names instances 
  

  
dfn <- df %>% 
  left_join(., state_labels %>% rename(State = StateID), by = "State") %>% 
  left_join(., breed_labels %>% select(Breed1 = BreedID, MainBreed = BreedName), by = "Breed1") %>% 
  left_join(., breed_labels %>% select(Breed2 = BreedID, SecondBreed = BreedName), by = "Breed2") %>% 
  left_join(., color_labels %>% select(Color1 = ColorID, ColorName1 = ColorName), by = "Color1") %>% 
  left_join(.,color_labels %>% select(Color2 = ColorID, ColorName2 = ColorName), by = "Color2") %>% 
  left_join(.,color_labels %>% select(Color3 = ColorID, ColorName3 = ColorName), by = "Color3")%>% 
  mutate(Type = recode(Type, "1" = "Dog", "2" = "Cat"),
         Gender = recode(Gender, "1" = "Male", "2" = "Female","3" = "Mixed")) %>% # mixed has more than one pets, refers to quantity 
  mutate(MaturitySize = if_else(MaturitySize == 0, median(df$MaturitySize), MaturitySize)) %>% 
  mutate(FurLength = if_else(FurLength == 0, median(df$FurLength), FurLength)) %>% 
  mutate(Health = if_else(Health == 0, median(df$Health),Health)) %>% 
  mutate(MaturitySize = recode(MaturitySize, "1" = "Small", "2" = "Medium", "3" = "Large", "4" = "Extra Large"),
         FurLength = recode(FurLength, "1" = "Short", "2" = "Medium", "3" = "Long",.default = NA_character_), # ordered variables
         Health = recode(Health, "1" = "Healthy", "2" = "MinorInjury", "3" = "SeriousInjury", .default = NA_character_)
         ) %>% 
  mutate(Vaccinated = if_else(Vaccinated == 1, TRUE, FALSE)) %>% 
  mutate(Dewormed = if_else(Dewormed == 1, TRUE,FALSE)) %>% 
  mutate(Sterilized = if_else(Sterilized == 1, TRUE, FALSE)) %>% 
  mutate(pure_breed = if_else(
    ((!is.na(MainBreed))& (!(MainBreed %in% not_pure))& (is.na(SecondBreed))) | ((!is.na(MainBreed))& (!(MainBreed %in% not_pure))& (MainBreed == SecondBreed)), TRUE, FALSE)) %>% # 1. MainBreed not in non_pure and secondBreed is NA; 2. MainBreed not in non_pure and main == second. 
  mutate(named = if_else(is.na(Name) | grepl(pattern = not_named, x = Name, ignore.case = TRUE), FALSE, TRUE)) %>% # no names is defined as NA or not_named-containing string
  mutate(pure_color = if_else(is.na(ColorName2)& is.na(ColorName3), TRUE, FALSE)) %>% # only ColorName1 has value is defined as pure color. 
  mutate(FOC = if_else(Fee == 0, TRUE, FALSE)) %>% 
  mutate(multimediaInfo = if_else(PhotoAmt == 0 & VideoAmt == 0, FALSE, TRUE)) %>% 
  mutate(groupedPet = if_else(as.numeric(Quantity) > 1, TRUE, FALSE)) %>% 
  mutate_at(vars("Type", "Gender", "MaturitySize","FurLength","Health", "StateName"), as.factor) %>% 
  mutate(  
    AdoptionSpeedL = recode(AdoptionSpeed,
                            "0" = 0,
                            "1" = 1,
                            "2" = 8,
                            "3" = 31,
                            "4" = 91),
    AdoptionSpeedR = recode(AdoptionSpeed,
                            "0" = 0.999,
                            "1" = 7.999,
                            "2" = 30.999,
                            "3" = 90.999,
                            "4" = Inf)) %>% 
  select(-RescuerID, -State, -Breed1, -Breed2, -Color1, -Color2, -Color3,-Name, -Quantity, -Fee, -VideoAmt, -Description, -PhotoAmt, -MainBreed, - SecondBreed, -ColorName1, -ColorName2, -ColorName3) %>% 
  select(PetID,AdoptionSpeedL, AdoptionSpeedR, everything()) 


kappa_cv <- rep(NA, fold)
classi_cv <- rep(NA, fold)

surv_cv <- function(fold, model, dist){
  set.seed(123)
  te_perc <- 1/fold
  te_response_perc <- round(table(dfn$AdoptionSpeed)*te_perc, 0)
  
  te_ind <- lapply(0:4, function(n){
    cbind(sample(rep(1:fold, length = length(seq_len(nrow(df))[dfn$AdoptionSpeed == n]))), 
          seq_len(nrow(df))[dfn$AdoptionSpeed == n])
  }) %>% do.call(rbind, .)
  

  for (i in seq_len(fold)){
    te <- dfn %>% 
      select(-PetID) %>% 
      .[te_ind[which(te_ind[,1] == i),2],]
    tr <- dfn %>% 
      select(-PetID,-AdoptionSpeed) %>% 
      .[te_ind[which(te_ind[,1]!= i),2],]
    
    fit <- ic_par(Surv(time = AdoptionSpeedL, time2 = AdoptionSpeedR, type = "interval2")~.,
                  data = tr, model = model, dist = dist) ## "lnorm", "loglogistic" seemes be better.
    pred <- predict(fit, newdata = te, type = "response")
    pred_cat <- case_when(
      pred <1 ~ 0,
      pred >= 1 & pred <8 ~ 1,
      pred >= 8 & pred <31 ~ 2,
      pred >= 31 & pred < 91 ~ 3,
      pred >= 91  ~ 4
    )
    
    classi_cv[i] <<- sum(pred_cat == te$AdoptionSpeed)/nrow(te)
    kappa_cv[i] <<- irr::kappa2(cbind(pred_cat, te$AdoptionSpeed), weight = "squared")$value
    
    
  }

}


surv_cv(fold = 10, model = "aft", dist = "weibull")
mean(kappa_cv); mean(classi_cv)

surv_cv(fold = 10, model = "ph", dist = "weibull")
mean(kappa_cv); mean(classi_cv)

surv_cv(fold = 10, model = "aft", dist = "exponential")
mean(kappa_cv); mean(classi_cv)

surv_cv(fold = 10, model = "ph", dist = "exponential")
mean(kappa_cv); mean(classi_cv)

surv_cv(fold = 10, model = "aft", dist = "lnorm")
mean(kappa_cv); mean(classi_cv)

surv_cv(fold = 10, model = "ph", dist = "lnorm")
mean(kappa_cv); mean(classi_cv)

surv_cv(fold = 10, model = "aft", dist = "loglogistic")
mean(kappa_cv); mean(classi_cv)

surv_cv(fold = 10, model = "ph", dist = "loglogistic")
mean(kappa_cv); mean(classi_cv)

surv_cv(fold = 10, model = "aft", dist = "gamma")
mean(kappa_cv); mean(classi_cv)

surv_cv(fold = 10, model = "ph", dist = "gamma")
mean(kappa_cv); mean(classi_cv)


## draw distribution 
s = 0.6 # WxH =  6.04 x 4.9
ten_real <- bind_cols(te, pred = pred_cat) %>% 
  count(AdoptionSpeed) %>% 
  mutate(perc = round(n/nrow(te)*100,1)) 
r <- ggplot(data = ten_real, aes(x=AdoptionSpeed, y=n)) +
  geom_bar(stat="identity", fill="black") +
  labs(x="", y="Number of pets") +
  theme_minimal()+
  geom_label(label=paste(ten_real$perc, "%"), size=3)
ggsave("../ppts/test_realLab.svg", plot = r, device = "svg", scale = s)


ten_pred <- bind_cols(te, pred = pred_cat) %>% 
  count(pred) %>% 
  mutate(perc = round(n/nrow(te)*100,1)) 
p <- ggplot(data = ten_pred, aes(x=factor(pred, levels = c(0:4)), y=n)) +
  geom_bar(stat="identity", fill="black") +
  scale_fill_discrete(drop=FALSE) + 
  scale_x_discrete(drop=FALSE)+ 
  labs(x="", y="Number of pets") +
  theme_minimal()+
  geom_label(label=paste(ten_pred$perc, "%"), size=3)
ggsave("../ppts/test_predLab.svg", plot = p, device = "svg", scale = s)