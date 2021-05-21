
full = lm(data = elem.df.2009,
          math.grade3.chisq ~
            (gamma.gini + 
            frac + 
            prop.school.white + 
            prop.school.black + 
            prop.school.hispanic + 
            home.owners + 
            med.income + 
            prop.school.lowincome + 
            prop.school.EL + 
            prop.white)^2)
          

step(full, k=7, direction = "backward") -> lmstep
summary(lmstep)

ela3 = lm(formula = ela.grade3.chisq ~ frac + prop.school.white + prop.school.black + 
            prop.school.hispanic + home.owners + med.income + prop.school.lowincome + 
            prop.school.EL + prop.white + frac:med.income + prop.school.black:home.owners + 
            prop.school.black:med.income + prop.school.hispanic:home.owners + 
            prop.school.hispanic:med.income + prop.school.hispanic:prop.school.lowincome + 
            prop.school.hispanic:prop.school.EL, data = elem.df.2009)

ela5 = lm(formula = ela.grade5.chisq ~ gamma.gini + frac + prop.school.white + 
            prop.school.black + prop.school.hispanic + home.owners + 
            med.income + prop.school.lowincome + prop.school.EL + prop.white + 
            gamma.gini:prop.school.white + frac:prop.school.white + frac:prop.school.black + 
            frac:prop.school.hispanic + prop.school.white:prop.school.lowincome + 
            prop.school.white:prop.school.EL + prop.school.white:prop.white + 
            prop.school.black:prop.school.hispanic + prop.school.black:home.owners + 
            prop.school.black:med.income + prop.school.black:prop.white + 
            prop.school.hispanic:home.owners + prop.school.hispanic:med.income + 
            prop.school.hispanic:prop.white, data = elem.df.2009)

math5 = lm(formula = math.grade5.chisq ~ gamma.gini + frac + prop.school.white + 
             prop.school.black + prop.school.hispanic + med.income + prop.school.lowincome + 
             prop.school.EL + prop.white + gamma.gini:med.income + frac:prop.school.white + 
             frac:prop.school.black + frac:prop.school.hispanic + frac:prop.school.lowincome + 
             frac:prop.school.EL + frac:prop.white + prop.school.white:prop.school.black + 
             prop.school.white:prop.white + prop.school.black:prop.school.hispanic + 
             prop.school.black:prop.white + prop.school.hispanic:prop.school.lowincome + 
             prop.school.hispanic:prop.school.EL + prop.school.hispanic:prop.white, 
           data = elem.df.2009)

math3 = lm(formula = math.grade5.chisq ~ gamma.gini + frac + prop.school.white + 
             prop.school.black + prop.school.hispanic + med.income + prop.school.lowincome + 
             prop.school.EL + prop.white + gamma.gini:med.income + frac:prop.school.white + 
             frac:prop.school.black + frac:prop.school.hispanic + frac:prop.school.lowincome + 
             frac:prop.school.EL + frac:prop.white + prop.school.white:prop.school.black + 
             prop.school.white:prop.white + prop.school.black:prop.school.hispanic + 
             prop.school.black:prop.white + prop.school.hispanic:prop.school.lowincome + 
             prop.school.hispanic:prop.school.EL + prop.school.hispanic:prop.white, 
           data = elem.df.2009)

ela3 = update(ela3, .~. - prop.school.hispanic:prop.school.lowincome) 
ela3 =  update(ela3, .~. - prop.school.hispanic:prop.school.EL)
ela3 =  update(ela3, .~. + prop.school.hispanic:home.owners)
ela3 =  update(ela3, .~. + prop.school.black:home.owners)
ela3 =  update(ela3, .~. + gamma.gini)
ela3 = update(ela3, .~. - prop.white)
ela3 = update(ela3, .~. - prop.school.EL)
ela3 = update(ela3, .~. - prop.school.white)

summary(ela3)
  
lm(formula = ela.grade3.chisq ~ frac + prop.school.black + prop.school.hispanic + 
     home.owners + med.income + prop.school.lowincome + gamma.gini + 
     frac:med.income + prop.school.black:med.income + prop.school.hispanic:med.income + 
     prop.school.hispanic:home.owners + prop.school.black:home.owners, 
   data = elem.df.2009) 
  
lm(formula = ela.grade5.chisq ~ frac + prop.school.black + prop.school.hispanic + 
     home.owners + med.income + prop.school.lowincome + gamma.gini + 
     frac:med.income + prop.school.black:med.income + prop.school.hispanic:med.income + 
     prop.school.hispanic:home.owners + prop.school.black:home.owners, 
   data = elem.df.2009)  %>% summary


summary(math3)
math3 = update(math3, . ~. -frac:prop.white - frac:prop.school.black - 
                 frac:prop.school.hispanic - frac:prop.school.white - frac:prop.school.lowincome -
                 frac:prop.school.lowincome - frac:prop.school.EL)
math3 = update(math3, .~.- prop.school.hispanic:prop.school.EL  -
                 prop.school.hispanic:prop.school.lowincome - prop.school.black:prop.white)
math3 = update(math3, .~.- prop.school.white:prop.white  -
                 prop.school.hispanic:prop.white)
summary(math3)


lm(formula = ela.grade3.chisq ~ gamma.gini + frac + prop.school.white + 
     prop.school.black + prop.school.hispanic + med.income + prop.school.lowincome + 
     prop.school.EL + prop.white + gamma.gini:med.income + prop.school.white:prop.school.black + 
     prop.school.black:prop.school.hispanic, data = elem.df) %>% summary


lm(formula = math.grade5.chisq ~ gamma.gini + frac + prop.school.white + 
     prop.school.black + prop.school.hispanic + med.income + prop.school.lowincome + 
     prop.school.EL + prop.white + gamma.gini:med.income + frac:prop.school.white + 
     frac:prop.school.black + frac:prop.school.hispanic + frac:prop.school.lowincome + 
     frac:prop.school.EL + frac:prop.white + prop.school.white:prop.school.black + 
     prop.school.white:prop.white + prop.school.black:prop.school.hispanic + 
     prop.school.black:prop.white + prop.school.hispanic:prop.school.lowincome + 
     prop.school.hispanic:prop.school.EL + prop.school.hispanic:prop.white, 
   data = elem.df.2009) %>% summary
