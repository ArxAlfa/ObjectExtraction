
pacman::p_load(pacman, rio, dplyr, grDevices, scatterplot3d)

# Import point cloud data

number <- "10"
fileName <- paste("./pcds/pcd", number, ".ply.csv", sep = "")
pcd <- rio::import(fileName)
pcd <- pcd[,2:7]

# Center the point cloud

for (i in 1:3)
{
    pcd[,i] <- pcd[,i] - mean(pcd[,i])
}

# Preprocess color data

RGB <- as.matrix(pcd[,4:6])

HSV <- t(
        as.data.frame(
            apply(RGB, 1, function(x) rgb2hsv(x, maxColorValue = 1))))

colnames(HSV) <- c("H", "S", "V")

# Color histogram

par(mfrow = c(3,1))

mains <- c("Hue", "Saturation", "Value")
plotColors <- c("#86c4dc", "#df9a93", "#9ea6b9")
lineColors <- c("#cf6a74", "#a15cda", "#e74c51")


for(i in 1:3)
{
    hist(HSV[,i],
         xlim = c(0, 1),
         ylim = c(0, 7),
         breaks = 15,
         main = mains[i],
         xlab = "",
         col = plotColors[i],
         freq = F
         )
    
    lines(density(HSV[,i], adjust = 3), col = lineColors[i], lwd = 3)
}

# Color bounding of the table

## Green filtering

H_LOWER_BOUND <- 4/15
H_UPPER_BOUND <- 6/15

GREENS <- HSV[HSV[,1] > H_LOWER_BOUND & HSV[,1] < H_UPPER_BOUND,]

par(mfrow = c(2,1))

for(i in 2:3)
{
    hist(GREENS[,i],
         xlim = c(0, 1),
         ylim = c(0, 10),
         breaks = 15,
         main = mains[i],
         xlab = "",
         col = plotColors[i],
         freq = F
    )
    
    lines(density(GREENS[,i], adjust = 3), col = lineColors[i], lwd = 3)
}

## Value and Hue Filtering

V_UPPER_BOUND = 12/15
S_LOWER_BOUND = 3/15

TABLE_COLORS <- GREENS[GREENS[,3] < V_UPPER_BOUND & GREENS[,2] > S_LOWER_BOUND, ]

INDEX <- as.numeric(rownames(TABLE_COLORS))

TABLE <- pcd[INDEX,]

# Align the Table with the Axes

pca <- prcomp(TABLE[,1:3],
             center = F,
             scale = F)

par(mfrow = c(1,1))

scatterplot3d(
    x = TABLE[,1],
    y = TABLE[,2],
    z = TABLE[,3],
    xlab = "x",
    ylab = "y",
    zlab = "z",
    color = c(rgb(TABLE[,4], TABLE[,5], TABLE[,6])))

rotated <- pcd

# Check if the z-axis has a positive orientation

if(pca$rotation[9] < 0)
{
    pca$rotation[7:9] <- -pca$rotation[7:9]
}

rotated[,1:3] <- as.matrix(pcd[,1:3]) %*% pca$rotation
TABLE[,1:3] <- as.matrix(TABLE[,1:3]) %*% pca$rotation

scatterplot3d(
    x = rotated[,1],
    y = rotated[,2],
    z = rotated[,3],
    xlab = "x",
    ylab = "y",
    zlab = "z",
    color = c(rgb(rotated[,4], rotated[,5], rotated[,6])))

# Filtering the top of the table

minQuantile <- 0.1
maxQuantile <- 0.9
xMax <- quantile(TABLE[,1], maxQuantile)
xMin <- quantile(TABLE[,1], minQuantile)
yMax <- quantile(TABLE[,2], maxQuantile)
yMin <- quantile(TABLE[,2], minQuantile)
zMin <- quantile(TABLE[,3], minQuantile)

object <- rotated[rotated[,1] > xMin & rotated[,1] < xMax
                 & rotated[,2] > yMin & rotated[,2] < yMax
                 & rotated[,3] > zMin ,]

scatterplot3d(
    x = object[,1],
    y = object[,2],
    z = object[,3],
    xlab = "x",
    ylab = "y",
    zlab = "z",
    color = c(rgb(object[,4], object[,5], object[,6])))

# Filter the green table

HSV_INDEX <- as.numeric(rownames(object))
HSV_OBJECT <- HSV[HSV_INDEX,]

object <- object[!(HSV_OBJECT[,3] < V_UPPER_BOUND & HSV_OBJECT[,2] > S_LOWER_BOUND
                 & HSV_OBJECT[,1] > H_LOWER_BOUND & HSV_OBJECT[,1] < H_UPPER_BOUND),]

scatterplot3d(
    x = object[,1],
    y = object[,2],
    z = object[,3],
    xlab = "x",
    ylab = "y",
    zlab = "z",
    color = c(rgb(object[,4], object[,5], object[,6])),
    grid = F)

# Clean the noise

kc <- kmeans(object[,1:2], 3)

sizes <- c(0,0,0)

for (i in 1:3)
{
    sizes[i] <- length(names(kc$cluster[kc$cluster == i]))
}

min_index <- which.min(sizes)

KC_INDEX <- names(kc$cluster[kc$cluster != min_index])

# Finally we get the object with reduced noise

final_object <- object[KC_INDEX,]

scatterplot3d(
    x = final_object[,1],
    y = final_object[,2],
    z = final_object[,3],
    xlab = "x",
    ylab = "y",
    zlab = "z",
    color = c(rgb(final_object[,4], final_object[,5], final_object[,6])),
    grid = F)

# Save
saveName <- paste("./pcds/obj/obj", number, ".csv", sep = "")
rio::export(final_object, saveName, "csv")

# Post cleaning

par(mfrow = c(1,1))

rm(list = ls())

pacman::p_unload(all)
