Davies.Harte.FD <- function (n, sigma = 1, delta = 0)

{
    N <- 2^ceiling(log2(n))
    M <- 2*N

    S.X.tau <- sigma^2 / pi * sin(pi*delta) * gamma(1-2*delta) *
        exp(lgamma((0:N)+delta) - lgamma((0:N)+1-delta))

    if (delta < 0)
        S.X.tau[1] <- -S.X.tau[1]

    S <- Re(fft(c(S.X.tau, S.X.tau[N:2])))

    Z <- rnorm(M)

    Y <- double(M)
        Y[1] <- Z[1] * sqrt(M*S[1])
        Y[2:N] <- (Z[2*(2:N)-1] + Z[2*(2:N)]*1i) * sqrt(M*S[2:N]/2)
        Y[N+1] <- Z[M] * sqrt(M*S[N+1])
        Y[(N+2):M] <- Conj(Y[N:2])

    Y <- Re(fft(Y, inverse = T)) / M

    return(Y[1:n])
}
