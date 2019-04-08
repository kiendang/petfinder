from sklearn.metrics import cohen_kappa_score, make_scorer

cohen_kappa_scorer = make_scorer(cohen_kappa_score, weights='quadratic')
