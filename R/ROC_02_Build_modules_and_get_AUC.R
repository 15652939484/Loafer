#' Combine modules.
#' #' 本页的函数是为了简化实际筛选模型/方法/参数以便获得更好的预测效果所做的尝试
#' 具体修改的主要有：同时提供训练集和验证集的AUC结果；部分方法会提供必要的参数；尝试了多种不同的方法。
#' 只要将各个模型的predict值输出就可以接ROC可视化了。
#' @param module_obj inheritParams from get_the_auc
#' @export
Combine_modules <- function(module_obj){
    ### 有些模型在效果不好（如使用随机数据+随机变量）的时候是可能报错的。
    pulled_df <- NULL
    try(pulled_df <- module.LR(module_obj) %>% `[[`(.,"res_df")  %>% rbind(pulled_df, .))
    try(pulled_df <- module.GB(module_obj) %>% `[[`(.,"res_df") %>% rbind(pulled_df, .) )
    try(pulled_df <- module.RF(module_obj)  %>% `[[`(.,"res_df") %>% rbind(pulled_df, .))
    try(pulled_df <- module.LR.step(module_obj)  %>% `[[`(.,"res_df") %>% rbind(pulled_df, .))
    try(pulled_df <- module.Bayes(module_obj)  %>% `[[`(.,"res_df") %>% rbind(pulled_df, .))
    try(pulled_df <- module.Lasso_or_Ridge(module_obj, Lasso_or_Ridge = "Lasso")  %>% `[[`(.,"res_df") %>% rbind(pulled_df, .))
    try(pulled_df <- module.Lasso_or_Ridge(module_obj, Lasso_or_Ridge = "Ridge")  %>% `[[`(.,"res_df") %>% rbind(pulled_df, .))
    try(pulled_df <- module.Elastic_Net(module_obj)  %>% `[[`(.,"res_df") %>% rbind(pulled_df, .))
    return(pulled_df)
}

#' building modules by using LR 使用LR进行建模
#' @param module_obj inheritParams from get_the_auc
#' @export
module.LR <- function(module_obj){
    set.seed(1234)
    ### get the module.
    module_obj$fit_train <- glm(module_obj$my_formula, data = module_obj$train, family = binomial())
    module_obj$method_note <- "LR" # method_note: a note about the module and method and param used.
    get_the_auc(module_obj)
}





#' building modules by using LR 使用LR进行建模
#' @param module_obj inheritParams from get_the_auc
#' @export
module.LR.step <- function(module_obj){
    set.seed(1234)
    ### get the module.
    fit <- glm(module_obj$my_formula, data = module_obj$train, family = binomial())
    module_obj$fit_train <-  step(fit, trace = F, direction="backward",criterion = "AIC",steps= 100000)
    module_obj$method_note <- "LR step AIC" # method_note: a note about the module and method and param used.

    get_the_auc(module_obj)
}



#' building modules by using LR 使用LR进行建模
#' @param module_obj inheritParams from get_the_auc
#' @inheritParams randomForest mtry, nodesize, ntree
#' @export
module.RF <- function(module_obj, mtry = 3, nodesize = 5, ntree = 10000){
    ### get the module.
    set.seed(1234) ## default seed
    module_obj$fit_train <- randomForest(module_obj$my_formula, data=module_obj$train,
                                         mtry = mtry, ntree = ntree,
                                         importance=TRUE, na.action=na.omit, nodesize = nodesize)
    module_obj$method_note <- "RF" %>% paste(.,"mtry:", mtry,"nodesize:", nodesize, "ntree", ntree) # method_note: a note about the module and method and param used.
    get_the_auc(module_obj)

}


#' building models by using gbm from gbm package
#' ### 暂时有问题用不了
#' export
#' @importFrom gbm gbm
#' @param module_obj inheritParams from get_the_auc
#' @export
module.GB <- function(module_obj){
    set.seed(1234)
    module_obj$fit_train <- gbm(module_obj$my_formula,
                                distribution = "bernoulli", ### 白努力分布，适用于分类变量。
                                # distribution = "huberized", ### 白努力分布，适用于分类变量。
                                data = module_obj$train,
                                n.trees = 1000,  ### 迭代次数， 整大点儿。
                                # interaction.depth = 5, ## 深度，越大，拟合效果越好，但是容易过拟合。
                                interaction.depth = 2, ## 深度，越大，拟合效果越好，但是容易过拟合。
                                cv.folds = 0, ## 交叉验证 = 7，设置后可以看到交叉验证的 错误率。
                                shrinkage = 0.03,  ## 学习率
                                n.minobsinnode =4)## 终节点个数
    module_obj$fit_train
    module_obj$method_note <- "GB" # method_note: a note about the module and method and param used.
    get_the_auc(module_obj)
}

#' Module with Bayes采用贝叶斯分类器进行计算
#' building modules by using LR 使用LR进行建模
#' @inheritParams get_the_auc module_obj
#' @param laplace param for smooth.
#' @export
module.Bayes <- function(module_obj, laplace = 0){
    module_obj$fit_train <- naiveBayes(module_obj$my_formula, data = module_obj$train, laplace = laplace)
    module_obj$method_note <- "Bayes" %>% paste(., "laplace:", laplace)
    get_the_auc(module_obj, method = "bayes")

}



#' get module by Lasso_or_Ridge使用岭回归进行分类分析。
#'
#' 参考资料： https://www.pluralsight.com/guides/linear-lasso-and-ridge-regression-with-r
#' @param module_obj inheritParams from get_the_auc
#' @param Lasso_or_Ridge character: Lasso or Ridge to set different methods.
#' @export

module.Lasso_or_Ridge <- function(module_obj, Lasso_or_Ridge = "Lasso"){
    ##
    if(Lasso_or_Ridge == "Lasso"){
        alpha = 1
    }else if(Lasso_or_Ridge == "Ridge"){
        alpha = 0
    }
    set.seed(1234) ## default seed
    lambdas <- 10 ^ seq(3, -4, by = -.1)
    ### 需要确保使用的变量具有一致性
    module_obj$x.train <- module_obj$train[,-ncol(module_obj$train)] %>% as.matrix()
    module_obj$x.test  <-  module_obj$test[,-ncol(module_obj$test)]  %>% as.matrix()
    module_obj$x.train <- colnames(module_obj$x.train) %in% module_obj$final_sig_v %>% module_obj$x.train[,.]
    module_obj$x.test <- colnames(module_obj$x.test) %in% module_obj$final_sig_v %>% module_obj$x.test[,.]
    module_obj$fit_train <- glmnet::glmnet(x = module_obj$x.train, y = module_obj$train$group, standardize = TRUE,
                                           alpha = alpha, family = 'gaussian', lambda = lambdas)
    # alpha = alpha, family = 'binomial', lambda = lambdas) ## 是否要为2分类变量专门设定方法？修改参数？
    ### 岭回归
    ### 查找最合适的 lambda
    cv_ridge <- glmnet::cv.glmnet(module_obj$x.train, module_obj$train$group, alpha = 0, lambda = lambdas)
    module_obj$lambda_best <- cv_ridge$lambda.min

    module_obj$method_note <- Lasso_or_Ridge
    ### 进行预测
    get_the_auc.Lasso_and_Ridge(module_obj)
}


#' building modules by using Elastic Net
#' 使用弹性网络进行建模。模型建模过程中内置了交叉验证：
#' 这可能降低过拟合使得验证集的结果变好，但是模型在训练集中的表现会变差。
#' @param module_obj inheritParams from get_the_auc
#' @param method resampling method
#' @param cv.repeats folds for cv.
#' @export
module.Elastic_Net <- function(module_obj, method = "repeatedcv", cv.repeats = 7){
    # Set training control
    set.seed(1234)
    ##
    train_cont <- trainControl(method = method,
                               number = 5, ## number for repeat or for resampling.
                               repeats = cv.repeats,
                               search = "random",
                               verboseIter = TRUE)

    ### 二元变量应当转换为 因子再进行分析。
    module_obj$train$group <- module_obj$train$group %>% factor
    module_obj$test$group  <- module_obj$test$group %>% factor
    # Train the model
    elastic_reg <- train(module_obj$my_formula,
                         data = module_obj$train,
                         method = "glmnet",
                         preProcess = c("center", "scale"),
                         # tuneLength = 5,
                         tuneLength = 5,
                         trControl = train_cont)
    module_obj$fit_train <- elastic_reg
    module_obj$method_note <- "Elastic Net" %>% paste(., "Method:", method, "cv folds:", cv.repeats)
    get_the_auc(module_obj, method = "Elastic Net")
}


# pred_num <- predict(module_obj$fit_train, newdata = module_obj$train)
#' get the auc of train and test data.
#' 可以为RF、LR、贝叶斯、弹性网络等模型提供AUC。
#' @param module_obj the obj for the moduling.
#' @param method to call different predict for different method; for some fit need different predict param. such as Bayes et.al.
#' @return res_df a data.frame contains method_note: a note about the module and method and param used.
#' @export
get_the_auc <- function(module_obj, method = "normal"){
    ### get the result.
    {### 训练集的AUC
        if(method == "bayes"){
            p <- predict(module_obj$fit_train, module_obj$train, type = "raw")
            pred_num <- p[,1]
        }else if(method == "normal"){
            pred_num <- predict(module_obj$fit_train, newdata = module_obj$train,
                                interval = 'prediction') ## 先计算概率
        }else if(method == "Elastic Net"){
            pred_num <- predict(module_obj$fit_train, newdata = module_obj$train,
                                type="prob") %>% `[`(., , 1)
        }
        real_I <-   module_obj$train$group
        AUC_ls <- roc(real_I, pred_num)
        AUC.train <- AUC_ls %>% `[[`(.,"auc") %>% sub("Area under the curve: ","",.) %>% as.numeric %>% round(., digits = 3)## roc 函数使用 factor形式即可。
        direction <- AUC_ls %>% `[[`(., "direction")
        ## 训练集确定方向后，验证集应当使用相同的方向才对。
        module_obj$for_plot$train$pred_num <- pred_num
        module_obj$for_plot$train$real_I <- real_I
        module_obj$for_plot$train$AUC_direction <- direction
    }

    {### 验证集的AUC
        if (method == "bayes") {
            p <- predict(module_obj$fit_train, module_obj$test, type = "raw")
            pred_num <- p[,1]
        }else if (method == "normal") {
            pred_num <- predict(module_obj$fit_train, newdata = module_obj$test,
                                interval = 'prediction') ## 先计算概率
        }else if(method == "Elastic Net"){
            pred_num <- predict(module_obj$fit_train, newdata = module_obj$test,
                                type="prob") %>% `[`(., , 1)
        }

        real_I <-   module_obj$test$group

        module_obj$for_plot$test$pred_num <- pred_num
        module_obj$for_plot$test$real_I <- real_I
        AUC.test <- roc(real_I, pred_num, direction = direction) %>% `[[`(.,"auc") %>% sub("Area under the curve: ","",.) %>% as.numeric %>% round(., digits = 3)## roc 函数使用 factor形式即可。
    }

    module_obj$res_df <- data.frame(method_note = module_obj$method_note, Var_in_module =
                                        module_obj$my_formula %>%
                                        as.character() %>% `[`(.,3), AUC.test, AUC.train,
                                    check = AUC.test > 0.7 & AUC.train < 1)
    return(module_obj)
}


#' building modules by using LR 使用LR进行建模
#' 为Lasso 和 Ridge 模型提供AUC
#' @param module_obj inheritParams from get_the_auc
#' @export
#  使用逐步回归进行分析建模。
get_the_auc.Lasso_and_Ridge <- function(module_obj){
    ### get the result.
    {### 训练集的AUC

        pred_num <- predict(module_obj$fit_train, newx = module_obj$x.train,
                            s = module_obj$lambda_best) %>% `[`(.,,1)## 先计算概率

        real_I <-   module_obj$train$group
        AUC_ls <- roc(real_I, pred_num)
        AUC.train <- AUC_ls %>% `[[`(.,"auc") %>% sub("Area under the curve: ","",.) %>% as.numeric %>% round(., digits = 3)## roc 函数使用 factor形式即可。
        direction <- AUC_ls %>% `[[`(., "direction")
        ## 训练集确定方向后，验证集应当使用相同的方向才对。

        module_obj$for_plot$train$pred_num <- pred_num
        module_obj$for_plot$train$real_I <- real_I
        module_obj$for_plot$train$AUC_direction <- direction
    }

    {### 验证集的AUC

        pred_num <- predict(module_obj$fit_train, newx = module_obj$x.test,
                            s = module_obj$lambda_best) %>% `[`(.,,1)## 先计算概率

        real_I <-   module_obj$test$group
        AUC.test <- roc(real_I, pred_num, direction = direction) %>% `[[`(.,"auc") %>% sub("Area under the curve: ","",.) %>% as.numeric %>% round(., digits = 3)## roc 函数使用 factor形式即可。

        module_obj$for_plot$test$pred_num <- pred_num
        module_obj$for_plot$test$real_I <- real_I
    }

    module_obj$res_df <- data.frame(method_note = module_obj$method_note, Var_in_module =
                                        module_obj$my_formula %>%
                                        as.character() %>% `[`(.,3), AUC.test, AUC.train,
                                    check = AUC.test > 0.7 & AUC.train < 1)
    return(module_obj)
}

