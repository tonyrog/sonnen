# Rest access to Sonnen battery

## A few commands

First the simple status check

    > sonnen:i("SB-103501.local", "8080").
	Consumption 515 W
	GridFeed 255 W
	Production 1.326000 Kw
	SOC 96%

Then read current battery status

	> sonnen:read_status("SB-103501.local").
	[{"apparent_output",908},
	[{backup_buffer,"0"},
	{battery_charging,true},
	{battery_discharging,false},
	{"consumption_avg",511},
	{consumption,511},
	{fac,49.97800064086914},
	{flow_consumption_battery,false},
	{flow_consumption_grid,false},
	{flow_consumption_production,true},
	{flow_grid_battery,false},
	{flow_production_battery,true},
	{flow_production_grid,true},
	{grid_feed,40},
	{is_system_installed,1},
	{operating_mode,"2"},
	{pac_total,-909},
	{production,1462},
	{rsoc,96},
	{"remainingcapacity_wh",9666},
	{"sac1",303},
	{"sac2",302},
	{"sac3",303},
	{system_status,"OnGrid"},
	{timestamp,"2022-09-25 16:22:35"},
	{usoc,96},
	{uac,226},
	{ubat,51}]
	
