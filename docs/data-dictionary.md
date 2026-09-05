# Data dictionary

| Field | Type / unit | Description |
|---|---|---|
| `date` | Date | Observation date |
| `hour` | Integer, 0–23 | Hour of day |
| `rented_bike_count` | Count | Hourly rentals; modeling target |
| `temperature_c` | °C | Air temperature |
| `humidity_pct` | Percent | Relative humidity |
| `wind_speed_ms` | m/s | Wind speed |
| `visibility_10m` | 10 m units | Visibility |
| `dew_point_c` | °C | Dew-point temperature |
| `solar_radiation_mj_m2` | MJ/m² | Solar radiation |
| `rainfall_mm` | mm | Hourly rainfall |
| `snowfall_cm` | cm | Hourly snowfall |
| `season` | Category | Winter, spring, summer, or autumn |
| `holiday` | Category | Holiday status |
| `functioning_day` | Yes / No | Whether the bicycle service operated |
| `weekday` | Category | Calendar weekday derived from `date` |
| `is_weekend` | Boolean | Saturday or Sunday indicator |

Source: [UCI Seoul Bike Sharing Demand](https://doi.org/10.24432/C5F62R), CC BY 4.0. Derived modeling fields include month and elapsed trend day.
