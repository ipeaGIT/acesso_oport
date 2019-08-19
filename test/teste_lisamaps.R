library(UScensus2000tract)
library(spdep)
library(ggplot2)
library(dplyr)

# load data
data("oregon.tract")
oregon.tract <- st_as_sf(oregon.tract)
# plot Census Tract map
plot(oregon.tract)

# create  Queens contiguity matrix
spatmatrix <- poly2nb(oregon.tract)

# create a neighbours list with spatial weights
listw <- nb2listw(spatmatrix)

# calculate the local moran of the distribution of white population
lmoran <- localmoran(oregon.tract$white, listw)
summary(lmoran)

# padronize the variable and save it to a new column
oregon.tract$s_white <- scale(oregon.tract$white)  %>% as.vector()

# create a spatially lagged variable and save it to a new column
oregon.tract$lag_s_white <- lag.listw(listw, oregon.tract$s_white)

# summary of variables, to inform the analysis
summary(oregon.tract$s_white)
summary(oregon.tract$lag_s_white)

# moran sccaterplot, in basic graphics (with identification of influential observations)
x <- oregon.tract$s_white
y <- oregon.tract$lag_s_white %>% as.vector()
xx <- data_frame(x, y)

moran.plot(x, listw)



# moran sccaterplot, in ggplot 
# (without identification of influential observations - which is possible but requires more effort)
ggplot(xx, aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = F) + 
  geom_hline(yintercept = 0, linetype = 'dashed') + 
  geom_vline(xintercept = 0, linetype = 'dashed') 

# create a new variable identifying the moran plot quadrant for each observation, dismissing the non-significant ones
oregon.tract$quad_sig <- NA

# high-high quadrant
oregon.tract[(oregon.tract$s_white >= 0 & 
                oregon.tract$lag_s_white >= 0) & 
               (lmoran[, 5] <= 0.05), "quad_sig"] <- "high-high"
# low-low quadrant
oregon.tract[(oregon.tract$s_white <= 0 & 
                oregon.tract$lag_s_white <= 0) & 
               (lmoran[, 5] <= 0.05), "quad_sig"] <- "low-low"
# high-low quadrant
oregon.tract[(oregon.tract$s_white >= 0 & 
                oregon.tract$lag_s_white <= 0) & 
               (lmoran[, 5] <= 0.05), "quad_sig"] <- "high-low"
# low-high quadrant
oregon.tract[(oregon.tract$s_white <= 0 
                   & oregon.tract$lag_s_white >= 0) & 
                    (lmoran[, 5] <= 0.05), "quad_sig"] <- "low-high"
# non-significant observations
oregon.tract[(lmoran[, 5] > 0.05), "quad_sig"] <- "not signif."  

oregon.tract$quad_sig <- as.factor(oregon.tract$quad_sig)
oregon.tract$id <- rownames(oregon.tract)

# plotting the map
df <- fortify(oregon.tract, region="id")
df <- left_join(df, oregon.tract)

df %>% 
  ggplot()+
  geom_sf(aes(fill = quad_sig), color = "white", size = 0.05)+
  # ggplot(aes(long, lat, group = group, fill = quad_sig)) + 
  # geom_polygon(color = "white", size = .05)  + 
  # coord_equal() + 
  theme_bw()+ 
  scale_fill_brewer( palette = "Set1")
