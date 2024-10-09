download.file("https://raw.githubusercontent.com/ghazkha/Assessment4/refs/heads/main/gene_expression.tsv", destfile = "gene_expression.tsv")

download.file("https://raw.githubusercontent.com/ghazkha/Assessment4/refs/heads/main/growth_data.csv", destfile = "growth_data.csv")

list.files()

gene_expression <- read.delim("gene_expression.tsv", header = TRUE, row.names = 1)
head(gene_expression)  # Hiển thị 6 dòng đầu tiên của dữ liệu

rm(gene_expression)

rowMeans(gene_expression) #Tính giá trị trung bình của các cột

meanofrow <- rowMeans(gene_expression) #gán giá trị đó một tên

gene_expression$Mean_Expression <- meanofrow #gắn giá trị đó vào bảng

table_of_values <- head(gene_expression, 6)  # Lấy 6 dòng đầu tiên
print(table_of_values)  # Hiển thị bảng

require(dplyr)

order(-gene_expression$Mean_Expression)

gene_expression[order(-gene_expression$Mean_Expression), ][1:10, ]

gene_expression <- read.delim("gene_expression.tsv", header = TRUE)
tengen<- strsplit(as.character(gene_expression$Name_Description), "_")

gene_identifiers <- do.call(rbind, tengen)


print(tengen)
rm(gene_identifiers)


# Giả sử gene_expression là data frame đã được định nghĩa
# Tách tiền tố và tên gen, chỉ nhận dấu "_" đầu tiên
tengen <- sapply(gene_expression$Name_Description, function(x) {
  # Thay thế dấu "_" đầu tiên bằng dấu "|" (hoặc một ký tự tạm khác)
  modified_string <- sub("_", "|", x, fixed = TRUE)
  
  # Tách chuỗi tại dấu "|" vừa thay thế
  parts <- strsplit(modified_string, "|", fixed = TRUE)[[1]]
  
  # Trả về gene identifier và gene name
  return(c(parts[1], parts[2])) # Giả sử chỉ có 2 phần, phần còn lại giữ nguyên
})

# Chuyển đổi kết quả thành data frame
result_df <- data.frame(Gene_Identifier = tengen[1, ], Gene_Name = tengen[2, ], stringsAsFactors = FALSE)

# Kiểm tra kết quả
print(result_df)


# reading the CSV file
growth_data <- read.csv("growth_data.csv")
# view column names
colnames(growth_data)

library(dplyr)


summary_stats <- growth_data %>%
  group_by(Site) %>%
  summarise(
    Mean_Circumf_2005 = mean(Circumf_2005_cm, na.rm = TRUE),
    SD_Circumf_2005 = sd(Circumf_2005_cm, na.rm = TRUE),
    Mean_Circumf_2020 = mean(Circumf_2020_cm, na.rm = TRUE),
    SD_Circumf_2020 = sd(Circumf_2020_cm, na.rm = TRUE)
  )

# Step 4: Print the summary statistics
print(summary_stats)

install.packages("tidyr")

library(dplyr)
library(tidyr)
library(ggplot2)

# Gather the data for Circumf_2005_cm and Circumf_2020_cm into long format
long_data <- growth_data %>%
  select(Site, Circumf_2005_cm, Circumf_2020_cm) %>%
  pivot_longer(cols = starts_with("Circumf"), 
               names_to = "Year", 
               values_to = "Circumference")

# Step 4: Create the box plot
ggplot(long_data, aes(x = Year, y = Circumference, fill = Site)) +
  geom_boxplot() +
  labs(title = "Box Plot of Tree Circumference at Start and End of Study",
       x = "Year",
       y = "Tree Circumference (cm)") +
  scale_x_discrete(labels = c("Circumf_2005_cm" = "2005", "Circumf_2020_cm" = "2020")) +
  theme_minimal()

# Load necessary libraries
library(ggplot2)

# Step 1: Create the box plot for Circumf_2005_cm at both sites
ggplot(growth_data, aes(x = Site, y = Circumf_2005_cm, fill = Site)) +
  geom_boxplot() +
  labs(title = "Box Plot of Tree Circumference in 2005 at Both Sites",
       x = "Site",
       y = "Tree Circumference (cm)")

# Create the box plot with adjusted y-axis limits and jittered points
ggplot(long_data, aes(x = Year, y = Circumference, fill = Site)) +
  geom_boxplot(alpha = 0.7) +  # Adjust the transparency of the boxes
  geom_jitter(color = "black", size = 0.5, width = 0.2, alpha = 0.5) +  # Add jittered points
  labs(title = "Box Plot of Tree Circumference at Start and End of Study",
       x = "Year",
       y = "Tree Circumference (cm)") +
  scale_x_discrete(labels = c("Circumf_2005_cm" = "2005", "Circumf_2020_cm" = "2020")) +
  theme_minimal() +
  ylim(0, max(long_data$Circumference, na.rm = TRUE) * 1.1)  # Adjust y-axis limits
