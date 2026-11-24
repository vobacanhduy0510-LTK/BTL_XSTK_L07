##############################################################################
# BTL XAC SUAT THONG KE (MT2013) - DE TAI 9: GIAO THONG
# Du lieu: Air Traffic Passenger Statistics - SFO (Kaggle: thedevastator)
# Chay tu tren xuong duoi. Ket qua: bang in ra console + thu muc output_BTL/
#   (bang .csv va hinh .png 300 dpi)
# Luu y: luu file o dang UTF-8 (RStudio: File > Save with Encoding > UTF-8)
#        hoac chay bang source("file.R", encoding = "UTF-8")
##############################################################################

# ---- 0. THIET LAP ----
if (getRversion() < "4.1.0") stop("Can R >= 4.1 de dung native pipe |>. Hay cap nhat R.")

goi_can  <- c("tidyverse", "car")
goi_thieu <- goi_can[!goi_can %in% rownames(installed.packages())]
if (length(goi_thieu) > 0) install.packages(goi_thieu)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(car)         # leveneTest
})
options(scipen = 999, pillar.sigfig = 8, width = 120)

CSV_PATH <- "Air_Traffic_Passenger_Statistics.csv"
if (!file.exists(CSV_PATH)) {
  if (interactive()) {
    message("Khong thay file trong: ", getwd(), " -> hay chon file csv.")
    CSV_PATH <- file.choose()
  } else {
    stop("Khong tim thay '", CSV_PATH, "' trong ", getwd(), ". Dung setwd() roi chay lai.")
  }
}

out_dir <- "output_BTL"
dir.create(out_dir, showWarnings = FALSE)

# ham tien ich: in hinh, luu png, luu bang csv
xuat_hinh <- function(p, ten, w = 8, h = 4.5) {
  print(p)
  ggsave(file.path(out_dir, ten), plot = p, width = w, height = h, dpi = 300)
}
xuat_bang <- function(bang, ten) {
  print(bang, n = Inf, width = Inf)
  write_excel_csv(bang, file.path(out_dir, ten))
}
giao_dien <- theme_bw(base_size = 13, base_family = "serif")

##############################################################################
# 1. DOC DU LIEU
##############################################################################
sfo_raw <- read_csv(CSV_PATH, show_col_types = FALSE)
cat("So dong:", nrow(sfo_raw), "| So cot:", ncol(sfo_raw), "\n")   # 15007 | 17
glimpse(sfo_raw)

##############################################################################
# 3. TIEN XU LY
##############################################################################
# 3.2 Du lieu khuyet
cat("\nSo NA theo cot (chi 2 cot ma IATA co NA):\n")
print(colSums(is.na(sfo_raw))[colSums(is.na(sfo_raw)) > 0])      # 54 + 54

# 3.3-3.6 Pipeline lam sach
sfo_clean <- sfo_raw |>
  select(-index) |>
  rename_with(~ str_replace_all(.x, " ", "_")) |>
  mutate(
    across(c(Operating_Airline, Published_Airline,
             Operating_Airline_IATA_Code, Published_Airline_IATA_Code), str_trim),
    Operating_Airline_IATA_Code = replace_na(Operating_Airline_IATA_Code, "Unknown"),
    Published_Airline_IATA_Code = replace_na(Published_Airline_IATA_Code, "Unknown"),
    Date             = ymd(paste0(Activity_Period, "01")),
    Quarter          = quarter(Date),
    is_International = if_else(GEO_Summary == "International", 1L, 0L),
    # Gop 2 nhan United (cung 1 hang, doi nhan do sap nhap Continental 07/2013).
    # KHONG gop Air Canada voi Air Canada Jazz (2 hang khac nhau).
    Airline_Group    = if_else(str_starts(Operating_Airline, "United Airlines"),
                               "United Airlines (gop)", Operating_Airline)
  )

cat("\nSo dong trung lap:", sum(duplicated(sfo_clean)), "\n")      # 0

# 3.7 Tap con suy dien: CHI "Enplaned" (7.016 quan trac doc lap).
#     KHONG loai Terminal "Other" o buoc nay (chi loai khi so sanh Terminal).
sfo_enplaned <- sfo_clean |> filter(Activity_Type_Code == "Enplaned")
cat("So quan trac sfo_enplaned:", nrow(sfo_enplaned), "\n")        # 7016

##############################################################################
# 4. THONG KE MO TA
##############################################################################

# ---- 4.1 Bang 2: Passenger Count theo loai hoat dong ----
mo_ta <- function(df) {
  df |> summarise(n = n(), trung_binh = mean(Passenger_Count),
                  do_lech_chuan = sd(Passenger_Count), trung_vi = median(Passenger_Count),
                  nho_nhat = min(Passenger_Count), lon_nhat = max(Passenger_Count))
}
bang2 <- bind_rows(
  sfo_clean |> mo_ta() |> mutate(nhom = "Toan bo du lieu"),
  sfo_clean |> group_by(nhom = Activity_Type_Code) |> mo_ta()
) |> relocate(nhom)
cat("\n--- BANG 2: Thong ke mo ta theo loai hoat dong ---\n")
xuat_bang(bang2, "bang2_mo_ta.csv")

# ---- 4.2 Hinh 1: xu huong theo thang ----
sfo_monthly <- sfo_enplaned |>
  group_by(Activity_Period) |>
  summarise(tong = sum(Passenger_Count), .groups = "drop") |>
  arrange(Activity_Period) |>
  mutate(Date = ymd(paste0(Activity_Period, "01")), t = row_number())

p1 <- ggplot(sfo_monthly, aes(x = Date, y = tong / 1e6)) +
  geom_line(color = "#1F4E78", linewidth = 1) +
  labs(x = "Thời gian", y = "Tổng lượt khách khởi hành (triệu người)") +
  giao_dien
xuat_hinh(p1, "hinh1_xu_huong.png")

# ---- 4.3 Bang 4 + Hinh 2: Top 10 hang bay ----
tong_theo_hang <- sfo_enplaned |>
  group_by(Airline_Group) |>
  summarise(tong_khach = sum(Passenger_Count), .groups = "drop") |>
  arrange(desc(tong_khach)) |>
  mutate(ty_le_pct = round(100 * tong_khach / sum(tong_khach), 2))
cat("\nSo hang bay sau khi gop United:", nrow(tong_theo_hang), "\n")   # 74

top10_airlines <- tong_theo_hang |> slice_head(n = 10) |> mutate(hang = row_number()) |> relocate(hang)
cat("\n--- BANG 4: Top 10 hang bay (Enplaned) ---\n")
xuat_bang(top10_airlines, "bang4_top10.csv")

p2 <- ggplot(top10_airlines, aes(x = reorder(Airline_Group, tong_khach), y = tong_khach / 1e6)) +
  geom_col(fill = "#2A5C8A") +
  coord_flip() +
  labs(x = NULL, y = "Tổng lượt khách khởi hành (triệu người), 2005-2016") +
  giao_dien
xuat_hinh(p2, "hinh2_top10.png")

# ---- 4.4 Hinh 3 (tron) va Hinh 4 (khu vuc) ----
ty_trong_geo <- sfo_enplaned |>
  group_by(GEO_Summary) |>
  summarise(so_ban_ghi = n(), tong_khach = sum(Passenger_Count),
            trung_binh = mean(Passenger_Count), do_lech_chuan = sd(Passenger_Count),
            .groups = "drop") |>
  mutate(ty_le_pct = round(100 * tong_khach / sum(tong_khach), 1))
cat("\n--- Ty trong Domestic / International ---\n")
xuat_bang(ty_trong_geo, "bang_geo.csv")   # Domestic 168.641.265 (77,2%); Intl 49.832.130 (22,8%)

p3 <- ggplot(ty_trong_geo, aes(x = "", y = tong_khach, fill = GEO_Summary)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(ty_le_pct, "%")), position = position_stack(vjust = 0.5),
            family = "serif", size = 5) +
  scale_fill_brewer(palette = "Set2", name = "Phạm vi") +
  theme_void(base_size = 13, base_family = "serif")
xuat_hinh(p3, "hinh3_ty_trong_geo.png", w = 5, h = 5)

p4 <- ggplot(sfo_enplaned, aes(x = reorder(GEO_Region, Passenger_Count, median),
                               y = Passenger_Count, fill = GEO_Region)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  coord_cartesian(ylim = c(0, quantile(sfo_enplaned$Passenger_Count, 0.97))) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Khu vực địa lý (GEO Region)", y = "Lượt khách khởi hành / tháng") +
  giao_dien +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
xuat_hinh(p4, "hinh4_khu_vuc.png")

# ---- 4.5 Hinh 5 + bang: phan loai gia ve ----
p5 <- ggplot(sfo_enplaned, aes(x = Price_Category_Code, y = Passenger_Count, fill = Price_Category_Code)) +
  geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
  coord_cartesian(ylim = c(0, quantile(sfo_enplaned$Passenger_Count, 0.97))) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Phân loại giá vé", y = "Lượt khách khởi hành / tháng") +
  giao_dien
xuat_hinh(p5, "hinh5_gia_ve.png", w = 6, h = 4.5)

bang_gia_ve <- sfo_enplaned |>
  group_by(Price_Category_Code) |>
  summarise(n = n(), trung_binh = mean(Passenger_Count), do_lech_chuan = sd(Passenger_Count),
            .groups = "drop")
cat("\n--- Thong ke theo phan loai gia ve ---\n")   # Low Fare 904 | 41.498,5 | 50.057,3 ; Other 6112 | 29.607,1 | 61.049,2
xuat_bang(bang_gia_ve, "bang_gia_ve.csv")

# ---- 4.6 Hinh 6 + bang: Terminal (loai "Other", n = 14) ----
sfo_terminal <- sfo_enplaned |>
  filter(Terminal != "Other") |>
  mutate(Terminal = factor(Terminal))
cat("\nSo quan trac sfo_terminal:", nrow(sfo_terminal), "\n")          # 7002

bang_terminal <- sfo_terminal |>
  group_by(Terminal) |>
  summarise(n = n(), trung_binh = mean(Passenger_Count), do_lech_chuan = sd(Passenger_Count),
            .groups = "drop") |>
  arrange(desc(trung_binh))
cat("\n--- Thong ke theo Terminal ---\n")   # T3 974 91.970,8 ; T2 192 89.360,1 ; T1 1561 35.374,1 ; Intl 4275 13.220,5
xuat_bang(bang_terminal, "bang_terminal.csv")

p6 <- ggplot(bang_terminal, aes(x = reorder(Terminal, -trung_binh), y = trung_binh, fill = Terminal)) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Blues") +
  labs(x = "Nhà ga (Terminal)", y = "Lượt khách khởi hành trung bình / tháng") +
  giao_dien
xuat_hinh(p6, "hinh6_terminal.png", w = 7, h = 4.5)

##############################################################################
# 5. THONG KE SUY DIEN (alpha = 0,05)
##############################################################################

# ---- 5.1 Khoang tin cay 95% cho trung binh ----
cat("\n=== 5.1 KHOANG TIN CAY 95% CHO TRUNG BINH ===\n")
ktc_tb <- t.test(sfo_enplaned$Passenger_Count, conf.level = 0.95)
print(ktc_tb)    # TB 31.139,31 ; KTC (29.738,0 ; 32.540,6)

# ---- 5.2 Welch: Domestic vs International ----
cat("\n=== 5.2 WELCH T-TEST: NOI DIA vs QUOC TE ===\n")
welch_geo <- t.test(Passenger_Count ~ GEO_Summary, data = sfo_enplaned, var.equal = FALSE)
print(welch_geo) # t = 30,46 ; df = 2741,7 ; KTC chenh lech (48.000,5 ; 54.605,0)

# ---- 5.3 Terminal: Levene, ANOVA, Kruskal-Wallis ----
cat("\n=== 5.3 SO SANH CAC TERMINAL ===\n")
levene_terminal <- leveneTest(Passenger_Count ~ Terminal, data = sfo_terminal)  # tam = median
print(levene_terminal)                                   # F = 662,63
anova_terminal <- aov(Passenger_Count ~ Terminal, data = sfo_terminal)
print(summary(anova_terminal))                           # F(3; 6998) = 677,23
kw_terminal <- kruskal.test(Passenger_Count ~ Terminal, data = sfo_terminal)
print(kw_terminal)                                       # H = 729,05 ; df = 3

# ---- 5.4 Chi-binh phuong: GEO Summary x Price Category ----
cat("\n=== 5.4 CHI-BINH PHUONG VE TINH DOC LAP ===\n")
bang_cheo <- table(GEO = sfo_enplaned$GEO_Summary, Gia_ve = sfo_enplaned$Price_Category_Code)
print(addmargins(bang_cheo))
print(round(100 * prop.table(bang_cheo, margin = 1), 1))  # ti le theo hang
chi_geo_gia <- chisq.test(bang_cheo)   # mac dinh R: hieu chinh Yates cho bang 2x2
print(chi_geo_gia)                     # X-squared = 1185,04 ; df = 1
cat("Tan so ky vong nho nhat:", round(min(chi_geo_gia$expected), 1), "(>= 5: dat dieu kien)\n")

# ---- 5.5 Hoi quy tuyen tinh don ----
cat("\n=== 5.5 HOI QUY TUYEN TINH DON: Tong khach ~ t ===\n")
mo_hinh_hqtt <- lm(tong ~ t, data = sfo_monthly)
print(summary(mo_hinh_hqtt))           # b0 = 1.309.562,8 ; b1 = 5.908,14 ; R2 = 0,6035
print(confint(mo_hinh_hqtt, level = 0.95))   # b1: (5.067,3 ; 6.749,0)

p7 <- ggplot(sfo_monthly, aes(x = t, y = tong / 1e6)) +
  geom_point(color = "#2A5C8A", alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#C0392B") +
  labs(x = "Tháng thứ t (t = 1, ..., 129)", y = "Tổng lượt khách khởi hành (triệu người)") +
  giao_dien
xuat_hinh(p7, "hinh7_hoi_quy.png")

# Hinh chan doan phan du (4 do thi)
png(file.path(out_dir, "hinh8_phan_du.png"), width = 2400, height = 2400, res = 300)
par(mfrow = c(2, 2)); plot(mo_hinh_hqtt); par(mfrow = c(1, 1))
dev.off()



cat("\nHOAN TAT. Bang va hinh nam trong thu muc:", normalizePath(out_dir), "\n")
