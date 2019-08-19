library(UScensus2000tract)
library(spdep)
library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)

# load data
access_for <- read_rds("../data/output_access/access_for.rds") %>%
  filter(!is.na(escolas_total))

# create  Queens contiguity matrix
spatmatrix <- poly2nb(access_for)

# create a neighbours list with spatial weights
listw <- nb2listw(spatmatrix)

# calculate the local moran of the distribution of white population
lmoran <- localmoran(access_for$escolas_total, listw)

# padronize the variable and save it to a new column
access_for$s_escolas <- scale(access_for$escolas_total)  %>% as.vector()

# create a spatially lagged variable and save it to a new column
access_for$lag_s_escolas <- lag.listw(listw, access_for$s_escolas)

# moran sccaterplot, in basic graphics (with identification of influential observations)
x <- access_for$s_escolas
y <- access_for$lag_s_escolas %>% as.vector()
xx <- tibble(x, y)

moran.plot(x, listw)



# moran sccaterplot, in ggplot 
# (without identification of influential observations - which is possible but requires more effort)
ggplot(xx, aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F) + 
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  geom_vline(xintercept = 0, linetype = 'dashed') 

# create a new variable identifying the moran plot quadrant for each observation, dismissing the non-significant ones
access_for$quad_sig <- NA

# high-high quadrant
access_for[(access_for$s_escolas >= 0 & 
                access_for$lag_s_escolas >= 0) & 
               (lmoran[, 5] <= 0.05), "quad_sig"] <- "high-high"
# low-low quadrant
access_for[(access_for$s_escolas <= 0 & 
                access_for$lag_s_escolas <= 0) & 
               (lmoran[, 5] <= 0.05), "quad_sig"] <- "low-low"
# high-low quadrant
access_for[(access_for$s_escolas >= 0 & 
                access_for$lag_s_escolas <= 0) & 
               (lmoran[, 5] <= 0.05), "quad_sig"] <- "high-low"
# low-high quadrant
access_for[(access_for$s_escolas <= 0 
                   & access_for$lag_s_escolas >= 0) & 
                    (lmoran[, 5] <= 0.05), "quad_sig"] <- "low-high"
# non-significant observations
access_for[(lmoran[, 5] > 0.05), "quad_sig"] <- "not signif."  

access_for$quad_sig <- as.factor(access_for$quad_sig)
access_for$id <- rownames(access_for)

# # plotting the map
# df <- fortify(access_for, region="id")
# df <- left_join(df, access_for)

access_for %>% 
  ggplot()+
  geom_sf(aes(fill = quad_sig), color = "black", size = 0.05)+
  # ggplot(aes(long, lat, group = group, fill = quad_sig)) + 
  # geom_polygon(color = "white", size = .05)  + 
  # coord_equal() + 
  theme_bw()+ 
  scale_fill_manual(values = c("red", "blue", "grey95"))
  scale_fill_brewer( palette = "Set1")
