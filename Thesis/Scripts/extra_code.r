# Match records to get ELA proficiency

new.col.names = c("prop.school.black", 
                  "prop.school.white",
                  "prop.school.hispanic",
                  "prop.school.asian",
                  "prop.school.pi",
                  "prop.school.multi",
                  "prop.school.disability",
                  "prop.school.EL",
                  "prop.school.lowincome",
                  "prop.school.homeless",
                  
                  "prop.ELA.proficient",
                  "prop.ELA.proficient.male",
                  "prop.ELA.proficient.female",
                  "prop.ELA.proficient.white",
                  "prop.ELA.proficient.black",
                  "prop.ELA.proficient.hispanic",
                  "prop.ELA.proficient.asian",
                  "prop.ELA.proficient.pi",
                  "prop.ELA.proficient.multirace",
                  "prop.ELA.proficient.lowincome",
                  "prop.ELA.proficient.homeless",
                  "prop.ELA.proficient.disabilities",
                  
                  "prop.math.proficient",
                  "prop.math.proficient.male",
                  "prop.math.proficient.female",
                  "prop.math.proficient.white",
                  "prop.math.proficient.black",
                  "prop.math.proficient.hispanic",
                  "prop.math.proficient.asian",
                  "prop.math.proficient.pi",
                  "prop.math.proficient.multirace",
                  "prop.math.proficient.lowincome",
                  "prop.math.proficient.homeless",
                  "prop.math.proficient.disabilities"
)

old.col.names = c("X..Student.Enrollment...Black.or.African.American", 
                  "X..Student.Enrollment...White",
                  "X..Student.Enrollment...Hispanic.or.Latino",
                  "X..Student.Enrollment...Asian",
                  "X..Student.Enrollment...Native.Hawaiian.or.Other.Pacific.Islander",
                  "X..Student.Enrollment...Two.or.More.Races",
                  "X..Student.Enrollment...Children.with.Disabilities",
                  "X..Student.Enrollment...EL",
                  "X..Student.Enrollment...Low.Income",
                  "X..Student.Enrollment...Homeless",
                  
                  "X..ELA.Proficiency.1",
                  "X..ELA.Proficiency...Male.1",
                  "X..ELA.Proficiency...Female.1",
                  "X..ELA.Proficiency...White.1",
                  "X..ELA.Proficiency...Black.or.African.American.1",
                  "X..ELA.Proficiency...Hispanic.or.Latino.1",
                  "X..ELA.Proficiency...Asian.1",
                  "X..ELA.Proficiency...Native.Hawaiian.or.Other.Pacific.Islander.1",
                  "X..ELA.Proficiency...Two.or.More.Races.1",
                  "X..ELA.Proficiency...Low.Income.1",
                  "X..ELA.Proficiency...Homeless.1",
                  "X..ELA.Proficiency...Children.with.Disabilities.1",
                  
                  "X..Math.Proficiency.1",
                  "X..Math.Proficiency...Male.1",
                  "X..MAth.Proficiency...Female.1",
                  "X..Math.Proficiency...White.1",
                  "X..Math.Proficiency...Black.or.African.American.1",
                  "X..Math.Proficiency...Hispanic.or.Latino.1",
                  "X..Math.Proficiency...Asian.1",
                  "X..Math.Proficiency...Native.Hawaiian.or.Other.Pacific.Islander.1",
                  "X..Math.Proficiency...Two.or.More.Races.1",
                  "X..Math.Proficiency...Low.Income.1",
                  "X..Math.Proficiency...Homeless.1",
                  "X..Math.Proficiency...Children.with.Disabilities.1"
)

# add general demographic school information
elem.df = merge_by_school(general, new.col.names, old.col.names)

\begin{pmatrix}
pop_{1} & 0 & \dots & 0 \\
0 & pop_{2} & \dots & 0\\
\vdots & \vdots & \ddots & \vdots \\
0 & 0 & \dots & pop_m\\
\end{pmatrix}
