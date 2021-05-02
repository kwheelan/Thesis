#=============================================================
# Process test score data on proficiency levels by school
#============================================================


testdata = read.delim("C:/Users/katri/Desktop/Thesis/School performance data/2009/all_data.txt", 
                         sep= ";", 
                         header=F, strip.white = T)[c("V3","V5", "V11")]

colnames(testdata) = c("Name","City", "Type")

grade3 = read.delim("C:/Users/katri/Desktop/Thesis/School performance data/2009/grade3_raw.txt", 
                    sep= ";", 
                    header=F)[,c(1:5,18:21)]
grade4 = read.delim("C:/Users/katri/Desktop/Thesis/School performance data/2009/grade4_raw.txt", 
                    sep= ";", 
                    header=F)[,c(2:5,18:21)]
grade5 = read.delim("C:/Users/katri/Desktop/Thesis/School performance data/2009/grade5_raw.txt", 
                    sep= ";", 
                    header=F)[,c(2:5,18:21)]

testdata[c("ID", "Grade3.ELA.Level1", "Grade3.ELA.Level2", "Grade3.ELA.Level3", "Grade3.ELA.Level4",
              "Grade3.Math.Level1", "Grade3.Math.Level2", "Grade3.Math.Level3", "Grade3.Math.Level4")] = grade3
testdata[c("Grade4.ELA.Level1", "Grade4.ELA.Level2", "Grade4.ELA.Level3", "Grade4.ELA.Level4",
              "Grade4.Math.Level1", "Grade4.Math.Level2", "Grade4.Math.Level3", "Grade4.Math.Level4")] = grade4
testdata[c("Grade5.ELA.Level1", "Grade5.ELA.Level2", "Grade5.ELA.Level3", "Grade5.ELA.Level4",
              "Grade5.Math.Level1", "Grade5.Math.Level2", "Grade5.Math.Level3", "Grade5.Math.Level4")] = grade5

testdata = testdata %>% filter(City == "Chicago") %>% filter(Type == "ELEMENTARY") 

write.csv(testdata, "C:/Users/katri/Desktop/Thesis/School performance data/2009/testdata_processed.txt")

read.csv("C:/Users/katri/Desktop/Thesis/School performance data/2009/testdata_processed.txt") -> testdata2009
