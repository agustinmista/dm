require(MASS)
require(class)
require(rpart)

#----------------------------------------#
#              GENERADORES               #
#----------------------------------------# 

# Genera datasets para el problema diagonal.
gen_diagonal <- function(dims=2, size=1000, C=0.5) {
    
    sdev <- C * sqrt(dims)
    cov <- diag(dims, dims, x=sdev^2)

    size0 <- floor(size / 2)  
    size1 <- size - size0  

    data0 <- cbind(mvrnorm(n=size0, rep(-1, dims), cov), matrix( 0, size0, 1)) 
    data1 <- cbind(mvrnorm(n=size1, rep(+1, dims), cov), matrix(+1, size1, 1))
    
    frame <- as.data.frame(rbind(data0, data1))
    colnames(frame)[dims+1] <- "class"

    return(frame)

}


# Genera datasets para el problema de espirales anidadas.
gen_spiral <- function (size=1000) {
    
    size0 <- floor(size / 2)  
    size1 <- size - size0  
    
    data0 <- t(replicate(size0, circle_sample(0)))
    data1 <- t(replicate(size1, circle_sample(1)))

    frame <- as.data.frame(rbind(data0, data1))
    colnames(frame) <- c("x", "y", "class") 

    return(frame)
}


# Genera un punto de manera uniforme dentro de la espiral dada por _class_.
circle_sample <- function (class) {
   
    sample <- sort(runif(2, 0, 1))
    
    x <- sample[2]*cos(2*pi*sample[1]/sample[2])
    y <- sample[2]*sin(2*pi*sample[1]/sample[2])
    
    if(classify(x, y) == class) 
        return(c(x, y, class))
    else
        circle_sample(class)
}


# Clasifica un punto dentro de las espirales anidadadas.
classify <- function (x, y) {

    rho <- function(x, y) { return(sqrt(x^2 + y^2)) }
    theta <- function(x, y) { return(atan2(y, x)) }
    
    below <- theta(x,y) / (4 * pi)    
    above <- (theta(x, y) + pi) / (4 * pi)

    inside <- function(n) { 
        rho(x,y) > below + 0.5 * n && rho(x,y) < above + 0.5 * n 
    }
    
    return(as.numeric(any(mapply(inside, 0:3))))
}


#----------------------------------------#
#                DATASETS                #
#----------------------------------------# 

diag_train <- gen_diagonal(dims=2, size=200, C=0.78)
diag_test <- gen_diagonal(dims=2, size=2000, C=0.78)

spiral_train <- gen_spiral(size=200)
spiral_test <- gen_spiral(size=2000)

#----------------------------------------#
#              GRAFICADORES              #
#----------------------------------------# 

plot_spiral <- function(spiral_data) {

    data0 <- subset(spiral_data, class == 0)
    data1 <- subset(spiral_data, class == 1)

    plot(data0$x, data0$y, col="red", xlim = c(-1,1), ylim = c(-1,1))
    points(data1$x, data1$y, col="blue") 

}

plot_diagonal <- function(diag_data) {
    
    data0 <- subset(diag_data, class == 0)
    data1 <- subset(diag_data, class == 1)

    minX <- min(data0$V1, data1$V1)
    maxX <- max(data0$V1, data1$V1)

    plot( data0$V1, data0$V2, col="red"
        , xlim = range(diag_data$V1)
        , ylim = range(diag_data$V2))
    points(data1$V1, data1$V2, col="blue") 

}

pdf("diag_data.pdf")
plot_diagonal(diag_test)
plot_diagonal(diag_train)

pdf("spiral_data.pdf")
plot_spiral(spiral_test)
plot_spiral(spiral_train)


#----------------------------------------#
#     CLASIFICACION : TRAIN Y TEST       #
#----------------------------------------# 

print("Error de clasificación usando conjuntos de train y test:")

# Ejecuta el algoritmo de k-vecinos más próximos variando el número de
# vecinos entre 1 y 100. Devuelve la tabla de error vs. cantidad de vecinos.
fit_knn <- function(train_data, test_data) {
    
    run_knn <- function(k) {
        res <- knn(train_data[,-3], test_data[,-3], train_data[,3], k=k )
        errors <- sum(res != test_data[,3])
        return(errors / length(test_data[,3]))
    }

    value <- 1:100 
    error <- sapply(value, run_knn)
    return(cbind(value, error))
}


# Obtenemos curvas de error vs. k para ambos problemas.
diag_knn <- fit_knn(diag_train, diag_test)
spiral_knn <- fit_knn(spiral_train, spiral_test)

pdf("diag_knn_fitting.pdf")
plot(diag_knn, type="l")
pdf("spiral_knn_fitting.pdf")
plot(spiral_knn, type="l")

# Elegimos los mejores valores de k para ambos problemas, quedándonos con el
# más chico en cada caso de haber valores repetidos. 
best_diag <- diag_knn[which(diag_knn[,2] == min(diag_knn[,2]))[1],]
best_spiral <- spiral_knn[which(spiral_knn[,2] == min(spiral_knn[,2]))[1], ] 

print(paste("Diagonal @ KNN:", best_diag[2], "( k =", best_diag[1], ")")) 
print(paste("Spiral @ KNN:", best_spiral[2], "( k =", best_spiral[1], ")")) 


# Calcula el error de clasificación usando árboles de decisión.
run_dtree <- function(train_data, test_data) {

    mod <- rpart(formula="class~.", data=train_data, method="class")
    pred <- predict(mod, test_data[,1:2], type="class")
    errors <- sum (pred != test_data[,3])

    return(errors/length(test_data[,3]))
}


diag_dtree_error <- run_dtree(diag_train, diag_test)
spiral_dtree_error <- run_dtree(spiral_train, spiral_test)

print(paste("Diagonal @ DT:", diag_dtree_error)) 
print(paste("Spiral @ DT:", spiral_dtree_error)) 


#----------------------------------------#
#    CLASIFICACION : CROSS VALIDATION    #
#----------------------------------------# 
print("Error de clasificación usando cross-validation:")

# Crea el fold número _fold_ a partir de un _dataset_ partido en _n_ folds.
make_fold <- function(n, fold, dataset) {

    data0 <- subset(dataset, class == 0)
    data1 <- subset(dataset, class == 1)

    fold_size0 <- nrow(data0)/n
    fold_size1 <- nrow(data1)/n

    start0 <- (fold - 1) * fold_size0
    start1 <- (fold - 1) * fold_size1
    end0   <- fold * fold_size0 - 1 
    end1   <- fold * fold_size1 - 1 

    fold_train <- rbind(data0[-(start0 : end0),], data1[-(start1 : end1),])
    fold_test  <- rbind(data0[start0 : end0,], data1[start1 : end1,])

    return(list("train"=fold_train, "test"=fold_test))
}


# Aplica un método _method_ de clasificación a un dataset, haciendo cross
# validation en _nfolds_.
cross_validate <- function(dataset, nfolds, method) {
    
    folds <- 1:nfolds

    apply_method <- function(k) {
        fold <- make_fold(nfolds, k, dataset)
        return(method(fold$train, fold$test)) 
    }
    errors <- sapply(folds, apply_method)
    return(mean(errors))
}


# Wrapper del metodo de clasificación de knn, con el fin de utilizar la misma
# interfaz de _cross_validate_ para ambos métodos.
knn_method <- function(train, test) {
    res <- knn(train[,-3], test[,-3], train[,3])
    errors <- sum(res != test[,3])
    return(errors / length(test[,3]))
}


# Ejecutamos el método de cross-validation sobre ambos problemas y ambos
# clasificadores.
diag_knn_cv <- cross_validate(diag_train, 5, knn_method)
spiral_knn_cv <- cross_validate(spiral_train, 5, knn_method)

diag_dtree_cv <- cross_validate(diag_train, 5, run_dtree)
spiral_dtree_cv <- cross_validate(spiral_train, 5, run_dtree)

print(paste("Diagonal @ KNN:", diag_knn_cv)) 
print(paste("Spiral @ KNN:", spiral_knn_cv)) 
print(paste("Diagonal @ DT:", diag_dtree_cv)) 
print(paste("Spiral @ DT:", spiral_dtree_cv)) 
