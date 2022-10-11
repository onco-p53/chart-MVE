# chart-MVE
R code to analyse event log temperature and nitrogen usage from Chart MVE TS controller liquid nitrogen tanks. Default logging is every 4 hours.

The output stats can be seen here: https://www.rhizobia.co.nz/r/N2-tanks.html

## Chart MVE TS Event Log Codes

| Event Code | Description                     |
|------------|---------------------------------|
| AM         | Alarm Mute                      |
| AH         | Temp A High Alarm               |
| AL         | Temp A Low Alarm                |
| BB         | Running on battery power        |
| BH         | Temp B High Alarm               |
| BL         | Temp B Low Alarm                |
| BP         | Bypass Sensor Open              |
| BV         | Low Battery Voltage Alarm       |
| BY         | Hot Gas Bypassing               |
| CA         | Temp A Calibration Alarm        |
| CB         | Temp B Calibration Alarm        |
| CG         | Bypass Sensor Calibration Alarm |
| CL         | OFAF Network Communication Loss |
| F          | Filling                         |
| FD         | Fill Disabled                   |
| FT         | Fill Time Alarm                 |
| HG         | Hot Gas Bypass Time Alarm       |
| LH         | High Level Alarm                |
| LL         | Low Level Alarm                 |
| LO         | Lid Open Alarm                  |
| PF         | Power Failure                   |
| UW         | Liquid Usage Warning Alarm      |
| ZO         | Level Zeroing                   |
