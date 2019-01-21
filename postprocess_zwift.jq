def tobool: if . == 0 then false else true end;

{ data: [.data[] | {
	zwid: .zwid | tostring,
	name,
	pos,
	race_time: .race_time[0],
	time_diff,
	bpm: .bpm[0],
	hrm: .hrm | tobool,
	power_type,
	watts: .watts[0],
	wkg: .wkg[0],
	wkg_ftp: .wkg_ftp[0]
}]
}
