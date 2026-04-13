library(ggplot2)

n2 <- c(600, 900, 1200)
U2 <- c(13, 19, 25)

df <- data.frame(n2, U2)

model <- lm(U2 ~ n2, data = df)

df$U2_fit <- fitted(model)
df$reziduali <- resid(model)

p1 <- ggplot(df, aes(n2, U2)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
        x = "Broj zavoja n2",
        y = expression(U[2] ~ "(V)")
    ) + theme_minimal()

print(p1)

p2 <- ggplot(df, aes(n2, reziduali)) +
    geom_point(size = 3) +
    geom_hline(yintercept = 0) +
    labs(
        x = "Broj zavoja n2",
        y = "Reziduali") + theme_minimal()

print(p2)