german$Duration_in_month<-as.numeric(german$Duration_in_month)
german$Credit_amount<-as.numeric(german$Credit_amount)
german$Installment_rate_in_percentage_of_disposable_income<-as.numeric(german$Installment_rate_in_percentage_of_disposable_income)
german$Present_residence_since<-as.numeric(german$Present_residence_since)
german$Age_in_Years<-as.numeric(german$Age_in_Years)
german$Number_of_existing_credits_at_this_bank<-as.numeric(german$Number_of_existing_credits_at_this_bank)
german$Number_of_people_being_liable_to_provide_maintenance_for<-as.numeric(german$Number_of_people_being_liable_to_provide_maintenance_for)
german$Default_status<-as.numeric(german$Default_status)

str(german)
fix(german)

dummy1<-data.frame(model.matrix(~(Status_of_existing_checking_account-1),data = german))
dummy2<-data.frame(model.matrix(~(Credit_history-1),data = german))
dummy3<-data.frame(model.matrix(~(Purpose-1),data = german))
dummy4<-data.frame(model.matrix(~(Savings_account-1),data = german))
dummy5<-data.frame(model.matrix(~(Present_employment_since-1),data = german))
dummy6<-data.frame(model.matrix(~(Personal_status_and_sex-1),data = german))
dummy7<-data.frame(model.matrix(~(Other_debtors-1),data = german))
dummy8<-data.frame(model.matrix(~(Property-1),data = german))
dummy9<-data.frame(model.matrix(~(Other_installment_plans-1),data = german))
dummy10<-data.frame(model.matrix(~(Housing-1),data = german))
dummy11<-data.frame(model.matrix(~(Job_status-1),data = german))
dummy12<-data.frame(model.matrix(~(Telephone-1),data = german))
dummy13<-data.frame(model.matrix(~(Foreign_worker-1),data = german))

fgerman<-cbind(german[,c(2,5,8,11,13,16,18,21)],dummy1,dummy2,dummy3,dummy4,dummy5,dummy6,dummy7,dummy8,dummy9,dummy10,dummy11,dummy12,dummy13)

set.seed(10)

indices<-sample(1:nrow(fgerman),0.7*(nrow(fgerman)))
train<-fgerman[indices,]
test<-fgerman[-indices,]

model1<-glm(Duration_in_month ~. ,data = train)

step<-stepAIC(model1,direction = "both")

summary(model1)
vif(model1)

model2<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
            - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
            - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
            - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
            - Job_statusA174 - TelephoneA192 - Foreign_workerA202,data = train)

summary(model2)
vif(model2)

idx <- order(coef(summary(model2))[,4])
out <- coef(summary(model2))[idx,]

model3<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
            - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
            - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
            - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
            - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
            ,data = train)

summary(model3)
vif(model3)

idx <- order(coef(summary(model3))[,4])
out <- coef(summary(model3))[idx,]

model4<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
            - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
            - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
            - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
            - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
            - PurposeA410,data = train)

summary(model4)
idx <- order(coef(summary(model4))[,4])
out <- coef(summary(model4))[idx,]

model5<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
            - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
            - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
            - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
            - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
            - PurposeA410 - Present_employment_sinceA74,data = train)

summary(model5)
idx <- order(coef(summary(model5))[,4])
out <- coef(summary(model5))[idx,]

model6<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
            - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
            - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
            - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
            - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
            - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
            ,data = train)

summary(model6)
idx <- order(coef(summary(model6))[,4])
out <- coef(summary(model6))[idx,]

model7<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
            - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
            - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
            - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
            - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
            - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
            - PropertyA121,data = train)

summary(model7)
idx <- order(coef(summary(model7))[,4])
out <- coef(summary(model7))[idx,]

model8<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
            - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
            - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
            - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
            - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
            - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
            - PropertyA121 - Status_of_existing_checking_accountA11,data = train)

summary(model8)
idx <- order(coef(summary(model8))[,4])
out <- coef(summary(model8))[idx,]

model9<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
            - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
            - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
            - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
            - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
            - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
            - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
            ,data = train)

summary(model9)
idx <- order(coef(summary(model9))[,4])
out <- coef(summary(model9))[idx,]

model10<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
            - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
            - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
            - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
            - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
            - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
            - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
            - Savings_accountA64,data = train)

summary(model10)
idx <- order(coef(summary(model10))[,4])
out <- coef(summary(model10))[idx,]

model11<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63,data = train)

summary(model11)
idx <- order(coef(summary(model11))[,4])
out <- coef(summary(model11))[idx,]

model12<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91,data = train)

summary(model12)
idx <- order(coef(summary(model12))[,4])
out <- coef(summary(model12))[idx,]

model13<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102,data = train)

summary(model13)
idx <- order(coef(summary(model13))[,4])
out <- coef(summary(model13))[idx,]

model14<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13,data = train)

summary(model14)
idx <- order(coef(summary(model14))[,4])
out <- coef(summary(model14))[idx,]

model15<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101,data = train)

summary(model15)
idx <- order(coef(summary(model15))[,4])
out <- coef(summary(model15))[idx,]

model16<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61,data = train)

summary(model16)
idx <- order(coef(summary(model16))[,4])
out <- coef(summary(model16))[idx,]

model17<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62,data = train)

summary(model17)
idx <- order(coef(summary(model17))[,4])
out <- coef(summary(model17))[idx,]

model18<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32,data = train)

idx <- round(summary(model18)$coef[summary(model18)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model19<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12,data = train)

idx <- round(summary(model19)$coef[summary(model19)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model20<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191,data = train)

idx <- round(summary(model20)$coef[summary(model20)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model21<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30,data = train)

idx <- round(summary(model21)$coef[summary(model21)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model22<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44,data = train)

idx <- round(summary(model22)$coef[summary(model22)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model23<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44 - Credit_historyA31,data = train)

idx <- round(summary(model23)$coef[summary(model23)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model24<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44 - Credit_historyA31 - Present_employment_sinceA71,data = train)

idx <- round(summary(model24)$coef[summary(model24)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model25<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44 - Credit_historyA31 - Present_employment_sinceA71
             - Credit_historyA33,data = train)

idx <- round(summary(model25)$coef[summary(model25)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model26<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44 - Credit_historyA31 - Present_employment_sinceA71
             - Credit_historyA33 - PropertyA122,data = train)

idx <- round(summary(model26)$coef[summary(model26)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model27<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44 - Credit_historyA31 - Present_employment_sinceA71
             - Credit_historyA33 - PropertyA122 - Other_installment_plansA142,data = train)

idx <- round(summary(model27)$coef[summary(model27)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model28<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44 - Credit_historyA31 - Present_employment_sinceA71
             - Credit_historyA33 - PropertyA122 - Other_installment_plansA142
             - Personal_status_and_sexA92,data = train)

idx <- round(summary(model28)$coef[summary(model28)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model29<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44 - Credit_historyA31 - Present_employment_sinceA71
             - Credit_historyA33 - PropertyA122 - Other_installment_plansA142
             - Personal_status_and_sexA92 - Personal_status_and_sexA93,data = train)

idx <- round(summary(model29)$coef[summary(model29)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model30<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44 - Credit_historyA31 - Present_employment_sinceA71
             - Credit_historyA33 - PropertyA122 - Other_installment_plansA142
             - Personal_status_and_sexA92 - Personal_status_and_sexA93 - PurposeA41,data = train)

idx <- round(summary(model30)$coef[summary(model30)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model31<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44 - Credit_historyA31 - Present_employment_sinceA71
             - Credit_historyA33 - PropertyA122 - Other_installment_plansA142
             - Personal_status_and_sexA92 - Personal_status_and_sexA93 - PurposeA41
             - PurposeA45,data = train)

idx <- round(summary(model31)$coef[summary(model31)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model32<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44 - Credit_historyA31 - Present_employment_sinceA71
             - Credit_historyA33 - PropertyA122 - Other_installment_plansA142
             - Personal_status_and_sexA92 - Personal_status_and_sexA93 - PurposeA41
             - PurposeA45 - PurposeA46,data = train)

idx <- round(summary(model32)$coef[summary(model32)$coef[,4] > .06, 4],4)
out <- data.frame(idx)

model33<-glm(Duration_in_month ~.- Number_of_people_being_liable_to_provide_maintenance_for
             - Status_of_existing_checking_accountA14 - Credit_historyA34 - PurposeA49
             - Savings_accountA65 - Present_employment_sinceA75 - Personal_status_and_sexA94
             - Other_debtorsA103 - PropertyA124 - Other_installment_plansA143 - HousingA153
             - Job_statusA174 - TelephoneA192 - Foreign_workerA202 - Present_residence_since
             - PurposeA410 - Present_employment_sinceA74 - Number_of_existing_credits_at_this_bank
             - PropertyA121 - Status_of_existing_checking_accountA11 - Other_installment_plansA141
             - Savings_accountA64 - Savings_accountA63 - Personal_status_and_sexA91
             - Other_debtorsA102 - Status_of_existing_checking_accountA13 - Other_debtorsA101
             - Savings_accountA61 - Savings_accountA62 - Credit_historyA32
             - Status_of_existing_checking_accountA12 - TelephoneA191 - Credit_historyA30
             - PurposeA44 - Credit_historyA31 - Present_employment_sinceA71
             - Credit_historyA33 - PropertyA122 - Other_installment_plansA142
             - Personal_status_and_sexA92 - Personal_status_and_sexA93 - PurposeA41
             - PurposeA45 - PurposeA46 - PurposeA43,data = train)

idx <- round(summary(model33)$coef[summary(model33)$coef[,4] > .06, 4],4)
out <- data.frame(idx)
summary(model33)

model34<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
              Age_in_Years + Default_status + PurposeA40 + PurposeA42 + PurposeA48 + 
              Present_employment_sinceA72 + Present_employment_sinceA73 + PropertyA123 + 
              HousingA151 + HousingA152 + Job_statusA171 + Job_statusA172 + Job_statusA173 + 
              Foreign_workerA201,data = train)

idx <- round(summary(model34)$coef[summary(model34)$coef[,4] > .06, 4],4)
out <- data.frame(idx)
summary(model33)

model35<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Age_in_Years + Default_status + PurposeA40 + PurposeA42 + 
               Present_employment_sinceA72 + Present_employment_sinceA73 + PropertyA123 + 
               HousingA151 + HousingA152 + Job_statusA171 + Job_statusA172 + Job_statusA173 + 
               Foreign_workerA201,data = train)

idx <- round(summary(model35)$coef[summary(model35)$coef[,4] > .06, 4],4)
out <- data.frame(idx)
summary(model35)

model36<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Age_in_Years + Default_status + PurposeA40 + PurposeA42 + 
               + Present_employment_sinceA73 + PropertyA123 + 
               HousingA151 + HousingA152 + Job_statusA171 + Job_statusA172 + Job_statusA173 + 
               Foreign_workerA201,data = train)

idx <- round(summary(model36)$coef[summary(model36)$coef[,4] > .06, 4],4)
out <- data.frame(idx)
summary(model36)

model37<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Age_in_Years + Default_status + PurposeA40 + PurposeA42 + PropertyA123 + 
               HousingA151 + HousingA152 + Job_statusA171 + Job_statusA172 + Job_statusA173 + 
               Foreign_workerA201,data = train)

summary(model37)

model38<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Age_in_Years + Default_status + PurposeA40 + PurposeA42 + 
              HousingA151 + HousingA152 + Job_statusA171 + Job_statusA172 + Job_statusA173 + 
               Foreign_workerA201,data = train)

summary(model38)

model39<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Age_in_Years + Default_status + PurposeA40 + PurposeA42 +  
               HousingA151 + Job_statusA171 + Job_statusA172 + Job_statusA173 + 
               Foreign_workerA201,data = train)

summary(model39)

model40<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Age_in_Years + Default_status + PurposeA40 + PurposeA42 +  
               Job_statusA171 + Job_statusA172 + Job_statusA173 + 
               Foreign_workerA201,data = train)

summary(model40)

model41<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Age_in_Years + Default_status + PurposeA40 + PurposeA42 +  
               Job_statusA171 + Job_statusA173 + 
               Foreign_workerA201,data = train)

summary(model41)

model42<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Age_in_Years + Default_status + PurposeA40 + PurposeA42 +  
               Job_statusA171 + Job_statusA173,data = train)

summary(model42)

model43<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Default_status + PurposeA40 + PurposeA42 +  
               Job_statusA171 + Job_statusA173,data = train)

summary(model43)

model44<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Default_status + PurposeA40 + Job_statusA171 + Job_statusA173,data = train)

summary(model44)

model45<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Default_status + PurposeA40 + Job_statusA171,data = train)

summary(model45)

model46<-glm(Duration_in_month ~ Credit_amount + Installment_rate_in_percentage_of_disposable_income +
               Default_status + PurposeA40,data = train)

summary(model46)

pred_train = predict(model46, data = train, type = "response")
rcorr.cens(pred_train, train$Default_status)
pred <- predict(model46, newdata = test)
roc_pred <- prediction(pred, test$Default_status)
model_perf <- performance(roc_pred, "tpr", "fpr")
plot(model_perf)
threshold = 0.35
pred_cf = predict(model46, newdata = test[,-7], type = "response")
table(pred_cf > threshold, test$Default_status)
table(pred_cf > 0.4, test$Default_status)
table(pred_cf > 0.5, test$Default_status)