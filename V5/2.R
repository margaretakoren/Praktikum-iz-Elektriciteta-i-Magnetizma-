library(ggplot2)
library(gridExtra)
library(grid)

U1 <- c(1.8, 4.0, 6.4, 8.8, 11.0, 13.0, 16.0)
U2 <- c(4.0, 8.2, 13.0, 17.0, 21.0, 25.0, 30.0)

df <- data.frame(U1 = U1, U2 = U2)
df$U2_U1 <- df$U2 / df$U1


n1 <- 300
n2 <- 600
teorijski_omjer <- n2 / n1


model <- lm(U2 ~ U1, data = df)
summary_model <- summary(model)

a <- coef(model)[2]
b <- coef(model)[1]

df$U2_fit <- fitted(model)
df$reziduali <- resid(model)


pearson_r <- cor(df$U1, df$U2, method = "pearson")
R2 <- summary_model$r.squared
odstupanje <- abs(a - teorijski_omjer) / teorijski_omjer * 100


cat("Tablica mjerenja:\n")
print(round(df, 3))
cat("\n")

cat("Jednadžba pravca:\n")
cat(sprintf("U2 = %.4f * U1 %+ .4f\n", a, b))
cat(sprintf("Teorijski omjer n2/n1 = %.3f\n", teorijski_omjer))
cat(sprintf("Pearsonov koeficijent r = %.4f\n", pearson_r))
cat(sprintf("Koeficijent determinacije R^2 = %.4f\n", R2))
cat(sprintf("Relativno odstupanje = %.2f %%\n", odstupanje))
cat("\n")

p1 <- ggplot(df, aes(x = U1, y = U2)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    x = expression(U[1] ~ "(V)"),
    y = expression(U[2] ~ "(V)")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank()
  )

print(p1)

p2 <- ggplot(df, aes(x = U1, y = reziduali)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linewidth = 1) +
  labs(
    x = expression(U[1] ~ "(V)"),
    y = "Reziduali"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank()
  )

print(p2)

tablica <- tableGrob(round(df, 3))

grid.newpage()
grid.draw(tablica)

ggsave("graf_glavni_transformator_600.pdf", plot = p1, width = 8, height = 5)
ggsave("graf_reziduali_transformator_600.pdf", plot = p2, width = 8, height = 4.5)

pdf("tablica_transformator_600.pdf", width = 8, height = 3.2)
grid.newpage()
grid.draw(tablica)
dev.off()