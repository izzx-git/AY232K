; cmd_uart
	; db "AT+UART_DEF=115200,8,1,0,3",13,10
; cmd_uart_end

; cmd_cwmode
	; db "AT+CWMODE=1",13,10
; cmd_cwmode_end

; cmd_cwjap
	; db "AT+CWJAP_DEF=\"��������\",\"�����\"",13,10
; cmd_cwjap_end

; cmd_savetrans_1
	; db "AT+SAVETRANSLINK=1,\"PC5\",8888,\"UDP\"",13,10
; cmd_savetrans_1_end

; cmd_savetrans_0
	; db "AT+SAVETRANSLINK=0",13,10
; cmd_savetrans_0_end

cmd_rst
	db "AT+RST",13,10
cmd_rst_end

cmd_brk
	db "+++"
cmd_brk_end

cmd_ipstart db 'AT+CIPSTART="UDP","' ;��� ������� � ����� ��������
cmd_ipstart_e


cmd_ipclose db "AT+CIPCLOSE",13,10
cmd_ipclose_e

cmd_ipsend_34 db "AT+CIPSEND=34",13,10
cmd_ipsend_34_e

cmd_ipsend_1058 db "AT+CIPSEND=1058",13,10
cmd_ipsend_1058_e

cmd_ipsend_290 db "AT+CIPSEND=290",13,10
cmd_ipsend_290_e

pack_id db "+IPD," ;����� ������ �� �������
pack_id_e

;������ ��������-�����
			; ld hl,trn_buf ;������
			; ld de,pack_size_32_com ;������
			; call Wifi.tcpSend ;���������
			; jr c, ;���� ������, �� ����� �������
			
			; ld hl,rec_buf ;����
			; ld (Wifi.buffer_pointer),hl
			; call Wifi.getPacket ;����
			; jr c, ;���� ������, �� ����� �������

