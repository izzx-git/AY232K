	LUA

	local nameflog = "DrvHDD.lua.info"
	local flog = nil
	local blname={}
	blname[1] ="DrvHDD.Start"
	blname[2] ="DrvHDD.End"

	if flog==nil then flog=io.open(nameflog,"w") end
	
	plen = sj.get_label(blname[1])
	flog:write("���� ��砫� Drv Hdd","\t"," = ",string.format('0x%04X', plen)," = ",plen,"\n")

	plen = sj.get_label(blname[2]) - sj.get_label(blname[1])
	flog:write("������ ����� �ࠩ���","\t"," = ",string.format('0x%04X', plen)," = ",plen,"\n")

	flog:flush ()

	ENDLUA


