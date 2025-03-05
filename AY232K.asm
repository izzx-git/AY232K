;AY232K (izzx)
;Клиент для обмена ZX-PC
    device ZXSPECTRUM128

	org #6000 
start_
col_fon equ 7 ;цвет фона
;col_len equ 9 ;атрибутов на имя файла
col_win equ 4*8+7 ;цвет окна сообщения
col_win2 equ 2*8+7 ;цвет окна сообщения
win_size equ 11 ;длина атрибутов окна сообщения
win_pos equ #5800+768/2+10+32+32 ;позиция окна сообщения
file_name_len equ 12 ;длина имени файла
lenghtName equ 16 ;длина имени файла в каталоге
scroll_len equ 13 ;длина области скрола сообщений
pack_size_1024_com equ 1024 ;длина основной части большого пакета
pack_size_1024 equ pack_size_1024_com+pack_size_32_com+2 ;длина пакета полная 
pack_size_1024_full equ pack_size_1024+40 ;длина большого пакета со служебной информацией
pack_size_1024_echo equ 26 ;длина ответа ESP на отправку большого пакета
pack_size_32_com equ 32 ;длина основной части малого пакета (заголовка)
pack_size_32 equ pack_size_32_com+2 ;длина пакета заголовка полная
pack_size_32_full equ pack_size_32+40 ;длина малого пакета со служебной информацией ??
pack_size_32_echo equ 24 ;длина ответа ESP на отправку малого пакета
pack_size_256_com equ 256 ;длина основной части пакета 256
pack_size_256 equ pack_size_256_com+pack_size_32_com+2 ;длина пакета полная 
pack_size_256_full equ pack_size_256+39 ;длина пакета 256 со служебной информацией
pack_size_256_echo equ 25 ;длина ответа ESP на отправку пакета 256
files_view equ 24-3 ;показывать файлов в каталоге
col_pos_l equ #5800 ;позиция для покраски левая панель
col_pos_r equ #5800+31-9 ;позиция для покраски правая панель
colorcatc_act equ 7*8+1 ;цвет выбранного файла активной панели
colorcat_ equ     1*8+7; фон панели файлов
retry_rec_count_max equ 50 ;число фреймов ожидания приёма байта
launch_name equ #5dd4 ;адрес имени файла в загрузчике для запуска
max_files equ 64 ;макс файлов в каталоге
max_files_trd equ 128 ;макс файлов в образе
;fix equ 500 ;дополнительная длина пакета для надёжного приёма
check_init_max equ 10 ;число отправки повторного пакета до внеочередного сброса устройства
cat_size_max equ 15*512 ;максимум буфер каталога слева


start
    ld      a,#10
    ld      (#5b5c),a ;sys var
	ld      a,(#5d19) ;текущий диск
    ld      (cur_drive) ,a
	ld a,"R"
	ld (panel),a ;выбрать правую панель
	
	; DI
	; ;установим прерывания 2
	; ld a,interrupt_vec/256 ;вектор
	; ld i,a
	; im 2
	; ld hl,interrupt ;адрес обработчика
	; ld ((interrupt_vec/256)*256+#ff),hl ;
	; ei
	call cls_pic
	call read_cfg	
	call init_dev
	call init_hdd ;проверить разделы FAT
start_h	
	call cls_pic
	ld hl,mes_title
	call print
	ld hl,mes_server
	call print
	ld hl,mes_port
	call print

	xor a ;обнулить переменные
	ld     (files),a
	ld     (shiftcat),a	
	ld     (curfile),a
	ld (cur_cat_pag),a
	ld (cur_cat_pag+1),a
	;call init_dev
	call renewcat_r
	call renewcat_l
	
wait ;ожидание клавиши
		call input_key
			
;waitPass
			ld      a,c ;вспомним клавишу
            cp 		"E"
            jp      z,exit
            cp 		6 ;#36-" "
            jp      z,down
            cp 		7 ;#37-" "
            jp      z,up
            cp 		"T" ;
            jp      z,TRD ;copy TRD
            cp 		"R"
            jp      z,readc
            cp 		"1"
            jp      z,driveA ;1
            cp 		"A"
            jp      z,driveA ;A			
            cp 		"2"
            jp      z,driveB ;2
            cp 		"B"
            jp      z,driveB ;B
            cp 		"3"
            jp      z,driveC ;3
            cp 		"C"
            jp      z,driveC ;C
            cp 		"4"
            jp      z,driveD ;4
            cp 		"D"
            jp      z,driveD ;D
			cp 		"H"
            jp      z,select_hdd ;снова на выбор HDD
            cp 		#0e ;-" " ;CS+SS
            jp      z,change_panel ;S
            cp 		5 ;#35-" "
            jp      z,left
            cp 		8 ;#38-" "
            jp      z,right
            ; cp 		"I" 
            ; jp      z,ShowInfo ;I
			; cp 		"S"
            ; jp      z,check_sum_tog ;S
            cp 		#0d
            jp      z,run_open ;enter	
			cp 		"I"
            jp      z,esp_rst ;сброс ESP
			cp 		"K"
            jp      z,mark_all ;отметить все
			cp 		"J"
            jp      z,unmark_all ;включить ESP в режим translink
            cp 		"5"
            jp      z,copyF ;копирование по файлам		
            cp 		" "
            jp      z,markF ;отметка файла
			
            jp      wait

readc	
			jp start_h
			

input_key ;на выходе в c - код клавиши
            halt
		
			ld      a,(keyLast) ;предыдущая нажатая клавиша
			ld		b,a
			ld 		a,(23556) ;сист. переменная нажатая клавиша 
			;ld 		c,a	
			call 	input_key_curs
			ld 		a,e
			ld 		c,a
			;ld      (23556),a
			ld 		(keyLast),a ;запомним клавишу
			cp		#ff ;если сейчас ничего не нажато
			jr		nz,wait1
			xor		a
			ld		(keyDelayF),a ;обнулим флаг задержки
			jr      input_key ;и снова на ожидание
wait1
			ld		a,c
			cp		b
			ret		nz ;если нажата первый раз, пропустим задержку

			ld 		a,(keyDelayF)
			or		a
			ret		nz ;если всё ещё нажата и была пауза, пропустим
			
;waitDelay		
			ld      a,(keyRepeat) ;пауза
			ld b,a
waitDelay
            halt
			ld 		a,(23556) ;сист. переменная нажатая клавиша
			call 	input_key_curs
			ld 		a,e
			;ld      (23556),a
			ld 		(keyLast),a ;запомним клавишу
			cp		#ff ;если сейчас ничего не нажато
			jr		nz,waitDelay1
			xor		a
			ld		(keyDelayF),a ;обнулим флаг задержки
			jr      input_key ;и снова на ожидание	
waitDelay1
			djnz waitDelay
			ld		a,1
			ld		(keyDelayF),a ;установить флаг была пауза
			jr      input_key ;и снова на ожидание	

input_key_curs ;перехват курсора
			ld e,a
			ld a,#fe 
			in a,(#fe)
			bit 0,a ;Caps 
			ret nz
			ld a,#f7 
			in a,(#fe)
			bit 4,a 
			jr nz,input_key_curs_1	
			ld e,5 ;left
			ret
input_key_curs_1				
			ld a,#ef 
			in a,(#fe)
			bit 4,a 
			jr nz,input_key_curs_2	
			ld e,6 ;down
			ret
input_key_curs_2				
			bit 3,a 
			jr nz,input_key_curs_3	
			ld e,7 ;up
			ret
input_key_curs_3				
			bit 2,a 
			jr nz,input_key_curs_4	
			ld e,8 ;right
			ret
input_key_curs_4				
			bit 0,a 
			ret nz	
			ld e,0 ;Back Space
			ret			
			
			
input_key_curs_e
			ld a,c
			ret


exit ;выход в DOS
            ld      a,#10
            ld      (#5b5c),a ;sys var
			LD 		HL,10072
			EXX
			;ld		bc,0
            ret
			
wait_cont ;обновить переменные и продолжить
	ld a,(panel)
	cp "L"
	jr nz,wait_cont_r
	ld     a,(shiftcat)
	ld     (shiftcat_l),a
	ld     a,(curfile)
	ld     (curfile_l),a
	call   printcat
    jp     wait
wait_cont_r
	ld     a,(shiftcat)
	ld     (shiftcat_r),a
	ld     a,(curfile)
	ld     (curfile_r),a
	call   printcat
    jp     wait	
			
down ; вниз по каталогу
            ld     a,(files)
            ld     c,a
			ld     a,(shiftcat)
			ld     b,a 		
            ld     a,(curfile)
            inc    a
            cp     c
            jp     nc,downE_upd ;подгрузка если дошли до конца
            ld     (curfile),a
			sub b
        ;    ld     a,(cursor)
            cp     files_view ;не пора ли сдвинуться вниз всему каталогу
            jr     c,downE
            inc b
            ld   a,b
            ld     (shiftcat),a
downE
            jp     wait_cont
downE_upd
			;подгрузка страницы каталога
			ld a,(panel)
			cp "R"
			jp nz,wait_cont ;только для правой панели
			ld a,(open_trd_flag)
			or a
			jp nz,wait_cont ;не при открытом образе			
			ld hl,(cur_cat_pag)
			inc hl
			ld (cur_cat_pag),hl
			xor a
			ld (curfile),a
			ld (shiftcat),a
			ld (files),a
			call cls_r
			call renewcat_r
			jp wait_cont


up ; ; вверх по каталогу

            ld     a,(curfile)
            or     a
            jp     z,upE_upd
            dec    a
            ld     (curfile),a
			ld     c,a
            ld     a,(shiftcat)
			ld b,a
			ld a,c
			sub b
			cp files_view
			jr c,upE
			dec b
			ld a,b
            ld     (shiftcat),a
upE
            jp     wait_cont
upE_upd
			;подгрузка страницы каталога
			ld a,(panel)
			cp "R"
			jp nz,wait_cont ;только для правой панели
			ld a,(open_trd_flag)
			or a
			jp nz,wait_cont ;не при открытом образе	
			ld hl,(cur_cat_pag)
			ld a,h
			or l
			jp z,wait
			dec hl
			ld (cur_cat_pag),hl
			xor a
			ld (curfile),a
			ld (shiftcat),a
			ld (files),a
			call cls_r
			call renewcat_r	
			jp wait_cont


left ; на страницу вверх

            ld     a,(curfile)
            or     a
            jp     z,wait
            sub    files_view-1
            jr     nc,left02
            xor    a
left02
            ld     (curfile),a
            ld     c,a
			;проверка сдвига
			ld     a,(shiftcat)
			ld b,a
			ld     a,c
			sub b
			;cp files_view
			jr nc,leftE ;листать не надо
			
			ld a,b
			sub files_view-1
			jr nc,left03
			xor a
left03
            ld     (shiftcat),a
leftE			
            jp     wait_cont




right; на страницу вниз
            ld     a,(files) ;всего
			or     a
            jp     z,wait
            ld     c,a
            ld     a,(curfile) ;текущий
            add    a,files_view-1 ;листаем вперёд
            cp     c
            jr     c,right01 ;если не дошли до конца
            ld     a,(files) ;иначе на последний файл
            dec    a
right01
            ld     (curfile),a

			;проверка сдвига, листать каталог или нет
			ld     a,(files)
			ld c,a
			cp files_view+1
			jr c,rightE ;выход если файлов меньше 25
            ld     a,(shiftcat)
			ld b,a
			ld     a,(curfile) ;текущий
			sub b
			cp files_view
			jr c,rightE ;если не пора пролистать на страницу
			ld a,files_view-1
			add b
			cp c ;если больше чем всего файлов
			jr c,right04
			ld a,c
			sub files_view ;тогда максимум файлов - 24
			jr right05
right04
			ld b,a ;и если до последнего файла не меньше 24х
			ld a,c
			sub b
			cp files_view
			ld a,b
			jr nc,right05
			ld a,c
			sub files_view ;тогда максимум файлов - 24

right05
            ld     (shiftcat),a ;новое смещение от начала каталога
rightE
            jp     wait_cont
			

; check_sum_tog ;переключение использовать контрольную сумму или нет
			; ld a,(check_sum_on)
			; or a
			; jr z,check_sum_tog3
			; xor a
			; ld (check_sum_on),a			
			; ld a,"N"			
			; ld (mes_check_sum),a
			; jr check_sum_tog1
			
; check_sum_tog3			
			; ld a,1
			; ld (check_sum_on),a
			; ld a,"Y"
			; ld (mes_check_sum),a
; check_sum_tog1
			; ld hl,mes_title
			; call print
			; call wait_releas
            ; jp     wait	
			
driveA
            ld      a,0
            ld      (#5d19) ,a
			ld 		(cur_drive),a
            ; ld      c,1
            ; call    #3d13
            ; ld      c,#18
            ; call    #3d13
            call    renewcat_l
            jp      wait
driveB
            ld      a,1
            ld      (#5d19) ,a
			ld 		(cur_drive),a
            ; ld      c,1
            ; call    #3d13
            ; ld      c,#18
            ; call    #3d13
            call    renewcat_l
            jp      wait
driveC
            ld      a,2
            ld      (#5d19) ,a
			ld 		(cur_drive),a
            ; ld      c,1
            ; call    #3d13
            ; ld      c,#18
            ; call    #3d13
            call    renewcat_l
            jp      wait
driveD
            ld      a,3
            ld      (#5d19) ,a
			ld 		(cur_drive),a
            ; ld      c,1
            ; call    #3d13
            ; ld      c,#18
            ; call    #3d13
            call    renewcat_l
            jp      wait
			
			
renewcat_l   ;обновить каталог слева
            xor     a
            ld      (curfile_l),a ;каталог перелистнуть в начало
            ld      (shiftcat_l),a 
			ld a,(panel)
			cp "L"
			jr nz,renewcat_l1
			xor a
            ld      (curfile),a ;
            ld      (shiftcat),a 
renewcat_l1			
			ld hl,cat_mark_l ;очистить таблицу отмеченных файлов
			ld de,cat_mark_l+1
			ld (hl),0
			ld bc,128-1
			ldir
			call cls_l
			;ld hl,mes_title
            ;call    printFI			
            call    readcat
            ; ld      a,(files)
            ; or      a
            ; jr      nz,renew01
            ; ld      hl,mesnofile
            ; call    print
; renew01
            call    printcat_l
            ; xor     a
            ; ld      (finmem),a ;no files in mem
			ld a,(panel)
			cp "L"
			ret nz
			ld     a,(files_l) ;обновить количество файлов
			ld     (files),a			

            ret		

			
renewcat_r ;обновить каталог справа
	call check_init_r ;авто проверка инициализации
	
;renewcat_r_
	ld hl,rec_buf ;очистить буфер
	ld de,rec_buf+1
	ld (hl),0
	ld bc,pack_size_32_com ;только начало ;
	ldir
			ld a,(23556) ;сист. переменная нажатая клавиша 
			cp " "
			jr nz,renewcat_r1 ;продолжим
			ld hl,mes_stopped ;прервём
			call print_sys
			call wait_releas
			jr cat_pass2
renewcat_r1
;запросим каталог
;WAITKEY	XOR A:IN A,(#FE):CPL:AND #1F:JR Z,WAITKEY

	ld a,0 ;Тип пакета Запроса каталога
	ld (trn_buf+3),a 
	ld de,(cur_cat_pag)
	ld (trn_buf+4),de ;смещение каталога (текущая страница)
	; ld (trn_buf+4),a ;смещение обнулить
	; ld (trn_buf+5),a
	;ld de,rec_buf ;где будет начало каталога пока точно не знаем
	; ld (rec_buf_adr_pack),de
	; ld (rec_buf_adr_pack1),de

	ld hl,trn_buf ;откуда
	ld de,pack_size_32_com ;размер
	call Wifi.tcpSend ;отправить
	jr c,renewcat_r ;сначала если не удачно
	
	ld hl,mes_req_cat
	call print_sys ;печать сообщения пока ждём ответ
	
	; ld b,1 ;подождать ещё, пока ESP сообразит
	; call pause1
	
	ld hl,rec_buf ;куда
	ld (Wifi.buffer_pointer),hl
	call Wifi.getPacket ;приём
	jr nc,cat_pass1
	;call init_dev
	jr renewcat_r ;сначала если не удачно
cat_pass1	
	ld hl,rec_buf
	ld bc,pack_size_1024_com+pack_size_32_com
	ld (check_sum_size),bc ;задать длину пакета для подсчёта
	ld c,1 ;Тип пакета Каталог
	call check_pack ;проверить пакет
	jr nc,cat_pass
	;call init_dev
	jr renewcat_r ;сначала если не удачно
cat_pass
	ld hl,(cur_cat_pag)
	call toDecimal
	ld hl,decimalS+2 ;
	ld de,mes_rec_cat_num
	ld bc,3
	ldir
	ld hl,mes_rec_cat
	call print_sys ;печать сообщения
cat_pass2
	ld hl,rec_buf+32
	ld de,cat_r
	;ld (rec_buf_adr_pack1),de ;перенесём каталог из буфера приёма
	ld bc,cat_r_end-cat_r
	ldir

	xor a
	ld (curfile_r),a ;выберем первый файл
	ld (shiftcat_r),a ;в начало каталога
	ld (files_r),a	;всего
	ld (open_trd_flag),a	
	;посчитаем сколько файлов
	ld ix,cat_r ;тут принятый каталог
	ld hl,files_r
	ld de,16 ;шаг
	ld b,max_files ;цикл строк и ограничение на количество файлов
calc_cat
	ld a,(ix)
	or a
	jr z,calc_cat1 ;выход если в начале имени 0
	inc (hl) ;прибавим количество
	add ix,de
	djnz calc_cat
calc_cat1
	;call sel_right_pan ;выбор панели
	call printcat_r	
	
			ld a,(panel)
			cp "R"
			ret nz
			ld     a,(files_r) ;обновить количество файлов
			ld     (files),a
	ret
	


run_open ;запуск файла или открытие образа
    ld     a,(files)
    or     a      ;no files?
    jp     z,wait
	
	ld a,(panel)
	cp "L"
	jp z,open_left ;если в левой панели, то запуск файла
	
	
open_trd	;открытие образа справа
			ld a,(open_trd_flag)
			or a
			jr z,open_trd0 ;продолжить, если образ ещё не открыт
			
			ld a,(curfile) ;текущ файл
			or a
			jp nz,wait ;выход, если не на первом файле (..)
	;выход из образа
	ld hl,cat_mark_r	
	ld de,cat_mark_r+1
	ld (hl),0
	ld bc,128-1
	ldir
	;вернём позицию курсора до открытия образа
	ld a,(curfile_r2)
	ld (curfile),a ;
	ld (curfile_r),a ;
	ld a,(shiftcat_r2)
	ld (shiftcat),a ;
	ld (shiftcat_r),a ;
	ld a,(files_r2)
	ld (files_r),a
	ld (files),a
	xor a
	ld (open_trd_flag),a
	;вернём каталог сервера
	ld de,cat_r
	ld hl,cat_r2
	ld bc,max_files*16
	ldir
	call cls_r
	call printcat_r ;напечатаем каталог
	jp wait_cont
	
open_trd0				
	;проверить расширение
	ld a,(curfile) ;текущ файл
	ld bc,cat_r
			ld l,a
			ld h,0
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add hl,bc
	call format_name_net
	ld bc,9
	add hl,bc
	ld a,(hl)
	cp "t"
	jr nz,open_trd0_ckeck
	jr open_trd0_ckeck_ext_ok
open_trd0_ckeck	
	cp "T"
	jp nz,wait ;выход
	
	
open_trd0_ckeck_ext_ok			
	;сохраним каталог сервера
	ld hl,cat_r
	ld de,cat_r2
	ld bc,max_files*16
	ldir
	
			ld hl,cat_mark_r ;очистить таблицу отмеченных файлов
			ld de,cat_mark_r+1
			ld (hl),0
			ld bc,128-1
			ldir
		
			ld a,(curfile) ;текущ файл
			ld bc,cat_r
			ld l,a
			ld h,0
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add hl,bc
	
			ld de,trn_buf+16 ;перекинем имя файла в запрос
			ld bc,file_name_len
			ldir
			
			xor a
			ld (trn_buf+12),a ;размер файла 655360
			ld (trn_buf+13),a 
			ld (trn_buf+15),a 
			ld a,#a
			ld (trn_buf+14),a
			
			ld a,6 ;Тип пакета Запрос приёма файла 256 байт
			ld (trn_buf+3),a
			ld hl,0 ;счётчик частей
			ld (trn_buf+4),hl
			ld xl,9 ;частей всего цикл 
			ld xh,0 ;начать с сектора 0

open_trd3	;цикл	
	call check_init_r ;авто проверка инициализации
	
			ld a,xl
			ld l,a
			ld h,0
			call print_progress
	
			ld a,(23556) ;сист. переменная нажатая клавиша 
			cp " "
			jr nz,open_trd2 ;продолжим
			ld hl,mes_stopped ;прервём
			call print_sys
			call wait_releas
			jp start_h
			
open_trd2			

			ld hl,trn_buf ;откуда
			ld de,pack_size_32_com ;размер
			call Wifi.tcpSend ;отправить
			jr c,open_trd3 ;если ошибка, то новая попытка

			ld hl,mes_req_part_file ;сообщение пока ждём ответ
			call print_sys
			
			ld hl,rec_buf ;куда
			ld (Wifi.buffer_pointer),hl
			call Wifi.getPacket ;приём
			jr nc,open_check_pass1
			;call init_dev
			jr open_trd3 ;если ошибка, то новая попытка
			
open_check_pass1	;принят пакет
			ld hl,mes_rec_part_file ;сообщение
			call print_sys
			ld hl,rec_buf
	ld bc,pack_size_256_com+pack_size_32_com
	ld (check_sum_size),bc ;задать длину пакета для подсчёта
			ld c,7 ; Тип пакета Файл 256
			call check_pack
			jr nc,open_check_pass
			;call init_dev
			jr open_trd3 ;если ошибка, то новая попытка
			
open_check_pass	;проверка прошла

	ld a,xh ;какой сектор по счёту
	inc xh
	ld hl,rec_buf+32
	ld de,cat_open_trd+16 ;первый файл пропустим, он будет ".."
	add d ;прибавить смещение на сектор
	ld d,a
	ld bc,pack_size_256_com
	ldir ;перекинем часть каталога
	
			ld hl,(trn_buf+4) ;следующая честь файла
			inc hl	
			ld (trn_buf+4),hl
			
			dec xl
			jp nz,open_trd3
	
	
	;обработаем каталог
	ld a,"." ;в начале двоеточие для возврата
	ld (cat_open_trd),a
	ld (cat_open_trd+1),a
	xor a
	ld (cat_open_trd+2),a
	ld bc,13-3
	ld hl,cat_open_trd+2
	ld de,cat_open_trd+3
	ldir
	
	; ld ixl,max_files_trd+1 ;отформатируем для красоты имён
	; ld hl,cat_open_trd
	; ld de,cat_open_trd ;(rec_buf_adr_pack1) ;перенос обратно
; open_trd_frm	
	; push hl
	; push de
	; call formatName
	; pop de
	; ld bc,16
	; ldir
	; pop hl
	; ld bc,16
	; add hl,bc
	; dec ixl
	; jr nz,open_trd_frm
	
	;запомнить на каком файле были в правой панели
	ld a,(curfile)
	ld (curfile_r2),a ;
	ld a,(shiftcat)
	ld (shiftcat_r2),a ;
	ld a,(files)
	ld (files2),a
	ld a,(files_r)
	ld (files_r2),a

	xor a
	ld (curfile_r),a ;выберем первый файл
	ld (shiftcat_r),a ;в начало каталога
	ld (files_r),a	;всего
	ld     (shiftcat),a	
	ld     (curfile),a	
	ld a,1
	ld (open_trd_flag),a ;открыт образ	
	
	;посчитаем сколько файлов
	ld ix,cat_open_trd ;тут принятый каталог
	ld hl,files_r
	ld de,16 ;шаг
	ld b,max_files_trd+1 ;цикл строк и ограничение на количество файлов
open_trd_calc_cat
	ld a,(ix)
	or a
	jr z,open_trd_calc_cat1 ;выход если в начале имени 0
	inc (hl) ;прибавим количество
	add ix,de
	djnz open_trd_calc_cat
open_trd_calc_cat1

	call cls_r ;очистить правую панель
	call printcat_r	
	
	ld hl,mes_progress_off
	call print
	
			ld a,(panel)
			cp "R"
			jp nz,wait
			ld     a,(files_r) ;обновить количество файлов
			ld     (files),a

	jp wait


cls_r ;очистка правой панели
            xor     a
cls_r1
            push    af
            ld      (catposit_r+1),a
            ld      hl,catposit_r ;установим позицию печати
            call    print
			ld      hl,cat_space ;пробелами забьём
			ld 		bc,12 ;длина
            call    print_ ;печать одного имени файла
            pop     af
            inc     a
            cp      files_view
            jr      c,cls_r1
			ret

cls_l ;очистка левой панели
            xor     a
cls_l1
            push    af
            ld      (catposit_l+1),a
            ld      hl,catposit_l ;установим позицию печати
            call    print
			ld      hl,cat_space ;пробелами забьём
			ld 		bc,12 ;длина
            call    print_ ;печать одного имени файла
            pop     af
            inc     a
            cp      files_view
            jr      c,cls_l1
			ret

open_left ;enter в левой панели
	ld a,(cur_drive)
	cp 4
	jp nc,open_dir_l

launch_basic ;запуск бэйсик файла из левой панели
	
	;Подставим имя файла в загрузчик

			ld a,(curfile) ;текущ файл
			ld bc,cat_l
			ld l,a
			ld h,0
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add hl,bc
			push hl
			pop ix ;запомним
			ld bc,8
			add hl,bc
			ld a,(hl)
			cp "B"
			jp nz,wait ;если не бэйсик файл
			
	;ищем заготовленную бэйсик строку
	ld hl,#5b00
	ld bc,#8000-#5b00

launch_find
	ld a,"-" ;символ, который забит временно вместо имени
	cp (hl)
	jr nz,launch_find_next
	ld d,7 ;сколько символов сравнить
launch_find1	
	inc hl
	cp (hl)
	jr nz,launch_find_next
	dec d
	jr nz,launch_find1
	jr launch_find_ok ;нашли
	
launch_find_next
	inc hl
	dec bc
	ld a,b
	or c
	jr nz,launch_find	
	jp wait ;не нашли строки
			
launch_find_ok	
	ld bc,7 ;в начало имени
	and a
	sbc hl,bc
	ex de,hl
	push ix ;вспомним
	pop hl
	ld bc,8
	ldir	

	jp exit



; link_off ;попытка вывести ESP из режима моста translink
	; ld hl,mes_translink_off
	; call print_sys

	; ;call clear_input
	
	; ld hl,cmd_brk
	; ld de,cmd_brk_end-cmd_brk
	; call start_trn1 ;оправка	
	; call pause
	; ;call clear_input
	
	; ld hl,cmd_savetrans_0 ;выключение моста
	; ld de,cmd_savetrans_0_end-cmd_savetrans_0
	; call start_trn1 ;оправка	
	; call pause
	; ;call clear_input
	
	; ld hl,cmd_rst
	; ld de,cmd_rst_end-cmd_rst
	; call start_trn1
	; call pause
	; ;call clear_input

	; ld hl,mes_ok
	; call print_sys
	
	; jp wait

; link_on ;попытка включить ESP в режим моста translink
	; ld hl,mes_translink_on
	; call print_sys

	; ;call clear_input
	; halt
	; halt
	; ;call clear_input
	
	; ld hl,cmd_uart
	; ld de,cmd_uart_end-cmd_uart
	; call start_trn1
	; ld b,150
	; call pause1
	; ;call clear_input	
	
	; ld hl,cmd_cwmode
	; ld de,cmd_cwmode_end-cmd_cwmode
	; call start_trn1 ;оправка	
	; ld b,150
	; call pause1
	; ;call clear_input
	
	; ld hl,cmd_cwjap 
	; ld de,cmd_cwjap_end-cmd_cwjap
	; call start_trn1 ;оправка	
	; ld b,255
	; call pause1
	; ld b,255
	; call pause1
	; ;call clear_input
	
	; ld hl,cmd_savetrans_1
	; ld de,cmd_savetrans_1_end-cmd_savetrans_1
	; call start_trn1
	; ld b,150
	; call pause1
	; ;call clear_input
	
	; ld hl,cmd_rst
	; ld de,cmd_rst_end-cmd_rst
	; call start_trn1
	; ld b,150
	; call pause1
	; ;call clear_input
	
	; ld hl,mes_ok
	; call print_sys
	
	; jp wait


esp_rst ;сброс ESP
	ld hl,mes_esp_rst
	call print_sys

	;call clear_input
	
	ld hl,cmd_brk
	ld de,cmd_brk_end-cmd_brk
	call start_trn1 ;оправка
	ld b,5	
	call pause1
	;call clear_input
	
	ld hl,cmd_rst
	ld de,cmd_rst_end-cmd_rst
	call start_trn1
	ld b,5
	call pause1
	;call clear_input

	ld hl,mes_ok
	call print_sys

	jp wait	
	
pause	
	ld b,50 ;пауза секунду
pause1
	halt
	djnz pause1
	ret







copyF ;копирование по файлам
            ld     a,(files)
            or     a      ;если нет файлов там, где курсор, то выход
            jp     z,wait

			;определить панель
			ld a,(panel)
			cp "R"
			jp z,copyF_RL
			

			
copyF_LR	
			;из левой панели в правую файлы TRD
			
			ld a,(cur_drive)
			cp 4
			jp nc,copyF_LR_FAT ;если слева FAT
			

			;копирование файлов TRD слева направо		
			ld     a,(open_trd_flag)
            or     a      ;если не открыт образ справа, то выход
			jp 		z,wait
			
			
			;окно запроса с выбором диска
			ld a,(cur_drive)
			add a,"A"
			ld (mes_trd_dsk+3),a
			ld hl,mes_copy_file
			call print
			ld hl,mes_trd_dsk
			call print	
			ld a,col_win
			call paint_win
copyF_LR_w ;ожидание согласия
			halt
			ld 		a,(23556) ;сист. переменная нажатая клавиша 
			cp 		"N"
			jp z,copyF_exit ;выход если нет согласия
			cp 		"Y"
			jr nz,copyF_LR_w
			
			;узнать сколько отмеченных файлов
			ld hl,cat_mark_l
			ld b,128 ;размер таблицы
			ld xl,0 ;счётчик
copyF_LR_no_one_chk
			ld a,(hl)
			or a
			jr z,copyF_LR_no_one_chk1
			inc xl
copyF_LR_no_one_chk1
			inc hl
			djnz copyF_LR_no_one_chk
			inc xl
			dec xl
			jr nz,copyF_LR_no_one ;не один
			ld a,(curfile) ;тогда только текущий файл			
			call copyF_LR_one		
			jp nc,copyF_exit ;выход без ошибки
			cp 255
			jp z,copyF_exit ;выход после останова
			jp copyF_error ;выход с ошибкой
			
			
copyF_LR_no_one ;копировать несколько			
			ld hl,cat_mark_l
			ld a,xl
			ld (cat_mark_cur_tot),a
			xor a
copyF_LR_no_one_cl
			ld (cat_mark_cur_cl),a
			ld (cat_mark_cur),hl
			ld a,(hl) ;есть отметка?
			or a
			jr z,copyF_LR_no_one_cl1
			ld a,(cat_mark_cur_tot)
			ld l,a
			ld h,0
			dec a
			ld (cat_mark_cur_tot),a
			call print_progress_tot ;прогресс
			ld a,(cat_mark_cur_cl) ;очередной номер файла			
			call copyF_LR_one ;скопировать	
			jp nc,copyF_LR_no_one_cl1 ;продолжить без ошибки
			cp 255
			jp z,copyF_exit ;выход после останова
			jp copyF_error ;выход с ошибкой
			
copyF_LR_no_one_cl1			
			ld hl,(cat_mark_cur)
			ld (hl),0 ;снять отметку
			inc hl
			ld a,(cat_mark_cur_cl)
			inc a
			cp 128
			jr c,copyF_LR_no_one_cl
			
			jp copyF_exit



			
copyF_LR_one ;скопировать один файл слева направо
			;на входе в A - номер файла
			;на выходе C=1, если ошибка; A=255, если остановлено
			ex af,af'
			;узнать есть ли место на диске
			ld a,(cat_open_trd+8*256+#e4+16) ; общее количество файлов в образе
			cp 128
			jp nc,copyF_LR_error ;если уже максимум
			ex af,af'			
			;узнать параметры файла
			; ld de,0
			; ld      (#5ceb),de ;начало сектор дорожка
			ld bc,cat_l ;каталог диска слева
			;ld a,(curfile) ;текущ файл
			ld l,a
			ld h,0
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add hl,bc	

			ld (copyF_RL_source_file),hl ;указатель на запись о файле			
			ld bc,13
			add hl,bc
			ld a,(hl) ;объём файла в секторах
			ld (copyF_RL_source_sec_size),a ;
			ld c,a
			ld b,0	
			inc hl
			ld e,(hl)
			inc hl
			ld d,(hl)
			ld (copyF_RL_source_trk),de ;исходные дорожка-сектор
			ld (#5ceb),de ;отсюда начнём считывать
			
			ld hl,(cat_open_trd+8*256+#e5+16) ; количество свободных секторов в образе
			and a
			sbc hl,bc
			jp c,copyF_LR_error ;если не хватает места
			
			;цикл по размеру в секторах
			ld xl,a

			ld hl,(cat_open_trd+8*256+#e1+16) ;первые свободные сектор-дорожка назначения
			;узнаем где будет файл
			;пересчитать дорожки-секторы в пакеты
			ld c,l ;секторы
			ld b,0
			ld l,h ;дорожки
			ld h,0
			add hl,hl ;*2
			add hl,hl ;*4
			add hl,hl ;*8
			add hl,hl ;*16
			add hl,bc ;добавить секторы
			;ld hl,0 ;счётчик частей
			ld (trn_buf+4),hl
			
			;начинаем копировать файл
		
			ld a,(curfile_r2) ;текущ файл образа
			ld bc,cat_r2
			ld l,a
			ld h,0
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add hl,bc
	
			ld de,trn_buf+16 ;перекинем имя файла в запрос
			ld bc,file_name_len
			ldir
			ld a,8 ;Тип пакета Запрос на передачу файла 256 байт
			ld (trn_buf+3),a
			
			xor a
			ld (trn_buf+12),a ;размер файла 655360
			ld (trn_buf+13),a 
			ld (trn_buf+15),a 
			ld a,#a
			ld (trn_buf+14),a
			


			;цикл	
copyF_LR3			
			ld a,xl
			ld l,a
			ld h,0
			call print_progress
			
	ld hl,trn_buf+pack_size_32_com ;адрес отправляемого пакета
	ld de,(#5ceb)
	ld bc,#0105
	call #3d13 ;считать 1 сектор

	call sec256_to_srv ;отправить на сервер 
			
copyF_LR_check_pass	;проверка прошла

	ld de,(#5ceb)	
	ld b,1
	call calc_next_pos2 ;сменить позицию на 1 сектор

			ld hl,(trn_buf+4) ;следующая честь файла
			inc hl	
			ld (trn_buf+4),hl
			
			dec xl
			jp nz,copyF_LR3

			;теперь поправить каталог

			;запись о файле
			ld a,(cat_open_trd+8*256+#e4+16) ; общее количество файлов
			ld l,a ;узнать в каком секторе будет запись о файле
			ld h,0
			add hl,hl ;*2
			add hl,hl ;*4
			add hl,hl ;*8
			add hl,hl ;*16
			ld a,h ;запомнить номер сетора в каталоге
			ld (copyF_RL_dest_sec_cat),a
			ld bc,cat_open_trd+16
			add hl,bc ;здесь будет запись о новом файле
			ex de,hl
			
			ld hl,(copyF_RL_source_file) ;запись о файле
			ld bc,16
			ldir ;скопировать
			ex de,hl
			dec hl
			ld de,(cat_open_trd+8*256+#e1+16) ;первые свободные сектор-дорожка назначения
			ld (hl),d ;дорожка
			dec hl
			ld (hl),e ;сектор

			ld a,(copyF_RL_dest_sec_cat)
			ld h,a
			ld l,0
			ld bc,cat_open_trd+16 ;скопировать сектор с учётом первой записи ".."
			add hl,bc
			ld de,trn_buf+pack_size_32_com
			ld bc,256
			ldir
			
			ld h,0
			ld a,(copyF_RL_dest_sec_cat)
			ld l,a ;номер сектора
			ld (trn_buf+4),hl ;номер части
			;отправка сектора в образ
			call sec256_to_srv		
			
			;служебный сектор
			ld de,(cat_open_trd+8*256+#e1+16) ;первые свободные сектор-дорожка назначения
			ld a,(copyF_RL_source_sec_size) ;размер файла
			ld b,a
			call calc_next_pos2
			ld (cat_open_trd+8*256+#e1+16),de

			ld hl,(cat_open_trd+8*256+#e5+16) ; количество свободных секторов на диске
			ld a,(copyF_RL_source_sec_size) ;размер файла
			ld c,a
			ld b,0
			and a
			sbc hl,bc
			ld (cat_open_trd+8*256+#e5+16),hl
			
			ld hl,cat_open_trd+8*256+#e4+16 ; общее количество файлов			
			inc (hl)
			
			ld hl,cat_open_trd+8*256+16
			ld de,trn_buf+pack_size_32_com
			ld bc,256
			ldir
			
			;отправка сектора в образ
			ld hl,#0008
			ld (trn_buf+4),hl ;номер части			
			call sec256_to_srv

			ld     a,(files_r) ;обновить количество файлов
			inc a
			ld     (files_r),a

			;call cls_r ;очистить правую панель
			call printcat_l	
			call printcat_r	
			
			xor a ;С=0, нет ошибок
			ret


sec256_to_srv
			;отправка сектора 256 в образ. заголовок пакета уже должен быть готов
			call check_init_r ;авто проверка инициализации
			
			ld a,(23556) ;сист. переменная нажатая клавиша 
			cp " "
			jr nz,sec256_to_srv2 ;продолжим
			ld hl,mes_stopped ;прервём
			call print_sys
			call wait_releas
			pop hl ;поправить адрес возврата
			ld a,255 ;ошибка останов
			ccf ;С=1
			ret
			
sec256_to_srv2			
			
			
			ld de,trn_buf
			ld bc,pack_size_256_com+pack_size_32_com
			ld (check_sum_size),bc
			call calc_check_sum ;проверить сумму
			ld (trn_buf+pack_size_256_com+pack_size_32_com),hl ;записать её в пакет

			ld hl,trn_buf ;откуда
			ld de,pack_size_256 ;размер
			call Wifi.tcpSend ;отправить
			jr c,sec256_to_srv ;если ошибка, повтор
	
			ld hl,mes_trq_part_file ;сообщение пока ждём ответ
			call print_sys
			
			ld hl,rec_buf ;куда
			ld (Wifi.buffer_pointer),hl
			call Wifi.getPacket ;приём
			jr c,sec256_to_srv ;если ошибка, повтор
			
			;принят пакет
			ld hl,mes_trn_part_file ;сообщение
			call print_sys
			ld hl,rec_buf
			ld bc,pack_size_32_com
			ld (check_sum_size),bc ;задать длину пакета для подсчёта
			ld c,9 ; Тип пакета Файл 256
			call check_pack
			jr c,sec256_to_srv ;если ошибка]повтор
			xor a
			ret ;выход без ошибок

copyF_LR_error ;выход с ошибкой
			xor a
			ccf ;C=1
			ret





	
	
	
			
copyF_RL ;копирование файлов справа налево TRD

			ld a,(cur_drive)
			cp 4
			jp nc,copyF_RL_FAT ;если слева FAT
			
			ld a,(open_trd_flag)
			or a
			jp z,wait ;выход, если справа не открыт образ

			;узнать сколько отмеченных файлов
			ld hl,cat_mark_r
			ld b,128 ;размер таблицы
			ld xl,0 ;счётчик
copyF_RL_no_one_chk
			ld a,(hl)
			or a
			jr z,copyF_RL_no_one_chk1
			inc xl
copyF_RL_no_one_chk1
			inc hl
			djnz copyF_RL_no_one_chk
			inc xl
			dec xl
			jr nz,copyF_RL_no_one2 ;не ноль
			;не отмечено
			ld a,(curfile) ;текущ файл
			or a ;если стоит на ".."
			jp z,wait
copyF_RL_no_one2
			;окно запроса с выбором диска
			ld a,(cur_drive)
			add a,"A"
			ld (mes_trd_dsk+3),a
			ld hl,mes_copy_file
			call print
			ld hl,mes_trd_dsk
			call print	
			ld a,col_win
			call paint_win

copyF_RL_w ;ожидание согласия
			halt
			ld 		a,(23556) ;сист. переменная нажатая клавиша 
			cp 		"N"
			jp z,copyF_exit ;выход если нет согласия
			cp 		"Y"
			jr nz,copyF_RL_w
	
			inc xl
			dec xl
			jr nz,copyF_RL_no_one ;не один
			ld a,(curfile) ;тогда только текущий файл			
			call copyF_RL_one		
			jp nc,copyF_exit ;выход без ошибки
			cp 255
			jp z,copyF_exit ;выход после останова
			jp copyF_error ;выход с ошибкой	
			
			
copyF_RL_no_one ;копировать несколько			
			ld hl,cat_mark_r
			ld a,xl
			ld (cat_mark_cur_tot),a
			xor a
copyF_RL_no_one_cl
			ld (cat_mark_cur_cl),a
			ld (cat_mark_cur),hl
			ld a,(hl) ;есть отметка?
			or a
			jr z,copyF_RL_no_one_cl1
			ld a,(cat_mark_cur_tot)
			ld l,a
			ld h,0
			dec a
			ld (cat_mark_cur_tot),a
			call print_progress_tot ;прогресс
			ld a,(cat_mark_cur_cl) ;очередной номер файла			
			call copyF_RL_one ;скопировать	
			jp nc,copyF_RL_no_one_cl1 ;продолжить без ошибки
			cp 255
			jp z,copyF_exit ;выход после останова
			jp copyF_error ;выход с ошибкой			
copyF_RL_no_one_cl1			
			ld hl,(cat_mark_cur)
			ld (hl),0 ;снять отметку
			inc hl
			ld a,(cat_mark_cur_cl)
			inc a
			cp 128
			jr c,copyF_RL_no_one_cl
			
			jp copyF_exit


copyF_RL_one ;по одному файлу справа налево
			;на входе в A - номер файла
			;на выходе C=1, если ошибка; A=255, если остановлено
			ex af,af'
			;узнать есть ли место на диске
			ld a,(cat_l+8*256+#e4) ; общее количество файлов
			cp 128
			jp nc,copyF_RL_one_error ;если уже максимум
			ex af,af'			
			;узнать параметры файла
			; ld de,0
			; ld      (#5ceb),de ;начало сектор дорожка
			ld bc,cat_open_trd 
			;ld a,(curfile) ;текущ файл
			ld l,a
			ld h,0
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add hl,bc	

			ld (copyF_RL_source_file),hl ;указатель на запись о файле			
			ld bc,13
			add hl,bc
			ld a,(hl) ;объём файла в секторах
			ld (copyF_RL_source_sec_size),a ;
			ld c,a
			ld b,0	
			inc hl
			ld e,(hl)
			inc hl
			ld d,(hl)
			ld (copyF_RL_source_trk),de ;исходные дорожка-сектор
			
			ld hl,(cat_l+8*256+#e5) ; количество свободных секторов на диске
			and a
			sbc hl,bc
			jp c,copyF_RL_one_error ;если не хватает места
			
			;цикл по размеру в секторах
			ld xl,a

			ld de,(cat_l+8*256+#e1) ;первые свободные сектор-дорожка назначения
			ld (#5ceb),de
			;узнаем где находится искодный файл
			ld hl,(copyF_RL_source_trk) ;пересчитать дорожки-секторы в пакеты
			ld c,l ;секторы
			ld b,0
			ld l,h ;дорожки
			ld h,0
			add hl,hl ;*2
			add hl,hl ;*4
			add hl,hl ;*8
			add hl,hl ;*16
			add hl,bc ;добавить секторы
			;ld hl,0 ;счётчик частей
			ld (trn_buf+4),hl
			
			;начинаем копировать файл
		
			ld a,(curfile_r2) ;текущ файл образа
			ld bc,cat_r2
			ld l,a
			ld h,0
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add hl,bc
	
			ld de,trn_buf+16 ;перекинем имя файла в запрос
			ld bc,file_name_len
			ldir
			ld a,6 ;Тип пакета Запрос приёма файла 256 байт
			ld (trn_buf+3),a

			xor a
			ld (trn_buf+12),a ;размер файла 655360
			ld (trn_buf+13),a 
			ld (trn_buf+15),a 
			ld a,#a
			ld (trn_buf+14),a
			
			;цикл

copyF_RL3			
	call check_init_r ;авто проверка инициализации

			ld a,xl
			ld l,a
			ld h,0
			call print_progress
			
			ld a,(23556) ;сист. переменная нажатая клавиша 
			cp " "
			jr nz,copyF_RL2 ;продолжим
			ld hl,mes_stopped ;прервём
			call print_sys
			call wait_releas
			ld a,255
			ccf ;C=1
			ret
			
copyF_RL2			

			ld hl,trn_buf ;откуда
			ld de,pack_size_32_com ;размер
			call Wifi.tcpSend ;отправить
			jr c,copyF_RL3 ;если ошибка, то новая попытка
			
			ld hl,mes_req_part_file ;сообщение пока ждём ответ
			call print_sys
			
			ld hl,rec_buf ;куда
			ld (Wifi.buffer_pointer),hl
			call Wifi.getPacket ;приём
			jr nc,copyF_RL_check_pass1
			;call init_dev
			jr copyF_RL3 ;если ошибка, то новая попытка
			
copyF_RL_check_pass1	;принят пакет
			ld hl,mes_rec_part_file ;сообщение
			call print_sys
			ld hl,rec_buf
	ld bc,pack_size_256_com+pack_size_32_com
	ld (check_sum_size),bc ;задать длину пакета для подсчёта
			ld c,7 ; Тип пакета Файл 256
			call check_pack
			jr nc,copyF_RL_check_pass
			;call init_dev
			jp copyF_RL3 ;если ошибка, то новая попытка
			
copyF_RL_check_pass	;проверка прошла

	ld hl,rec_buf+32 ;адрес принятого пакета
	ld de,(#5ceb)
	ld bc,#0106
	call #3d13 ;запишем 1 сектор

	ld de,(#5ceb)	
	ld b,1
	call calc_next_pos2 ;сменить позицию на 1 сектор

			ld hl,(trn_buf+4) ;следующая честь файла
			inc hl	
			ld (trn_buf+4),hl

			dec xl
			jp nz,copyF_RL3

			;теперь поправить каталог

			;запись о файле
			ld a,(cat_l+8*256+#e4) ; общее количество файлов
			ld l,a ;узнать в каком секторе будет запись о файле
			ld h,0
			add hl,hl ;*2
			add hl,hl ;*4
			add hl,hl ;*8
			add hl,hl ;*16
			ld a,h ;запомнить номер сетора в каталоге
			ld (copyF_RL_dest_sec_cat),a
			ld bc,cat_l
			add hl,bc ;здесь будет запись о новом файле
			ex de,hl
			
			ld hl,(copyF_RL_source_file) ;запись о файле
			ld bc,16
			ldir ;скопировать
			ex de,hl
			dec hl
			ld de,(cat_l+8*256+#e1) ;первые свободные сектор-дорожка назначения
			ld (hl),d ;дорожка
			dec hl
			ld (hl),e ;сектор
			
			ld l,0 ;записать сектор целиком по ровному адресу
			ld d,0
			ld a,(copyF_RL_dest_sec_cat)
			ld e,a ;номер сектора
			ld bc,#0106 ;1 сектор записать
			call #3d13			
			
			;служебный сектор
			ld de,(cat_l+8*256+#e1) ;первые свободные сектор-дорожка назначения
			ld a,(copyF_RL_source_sec_size) ;размер файла
			ld b,a
			call calc_next_pos2
			ld (cat_l+8*256+#e1),de

			ld hl,(cat_l+8*256+#e5) ; количество свободных секторов на диске
			ld a,(copyF_RL_source_sec_size) ;размер файла
			ld c,a
			ld b,0
			and a
			sbc hl,bc
			ld (cat_l+8*256+#e5),hl
			
			ld hl,cat_l+8*256+#e4 ; общее количество файлов			
			inc (hl)
			
			ld hl,cat_l+8*256
			ld de,#0008
			ld bc,#0106 ;1 сектор записать
			call #3d13
			
			ld     a,(files_l) ;обновить количество файлов
			inc a
			ld     (files_l),a

			call printcat_l	
			call printcat_r
			xor a ;C=0
			ret
	
	
copyF_exit	;общий выход из копирования файлов		
			ld a,col_fon ;стереть окно
			call paint_win
			;call cls_l ;очистить левую панель
			ld hl,mes_win_cls ;стереть текст
			call print

			ld hl,mes_progress_off ;стереть прогресс
			call print
			ld hl,mes_progress_tot_off ;стереть общий прогресс
			call print			
			call printcat_l	
			call printcat_r			
			jp wait


copyF_error ;общий выход из копирования файлов с ошибкой
			ld a,col_win2
			call paint_win ;окно ошибки
			ld hl,mes_copy_error
			call print
			call wait_releas ;ждать клавишу
copyF_error1
			halt
			ld 	a,(23556) ;сист. переменная нажатая клавиша 
			cp #ff
			jr z,copyF_error1
			jr copyF_exit


copyF_RL_one_error
			xor a 
			ccf ;C=1
			ret


exit_h ;выход после операции с ожиданием клавиши
			halt
			ld 		a,(23556) ;сист. переменная нажатая клавиша 
			cp #ff
			jr z,exit_h
			jp start_h ;на горячий старт










TRD ;выбор записи или считывания
	ld a,(cur_drive)
	cp 4
	jp     nc,wait ;когда слева FAT, не работает копирование TRD

	ld a,(open_trd_flag)
	or a
	jp     nz,wait ;когда открыт образ справа, нельзя копировать целиком образ
	
			ld a,(panel)
			cp "R"
			jp z,write_trd_
	
	
read_trd_ ;прочитать TRD с диска на сервер
            ld     a,(files)
            or     a      ;no files?
            jp     z,wait
			
			;запросить имя образа
			ld hl,cat_l+8*256+#f5 ;имя диска тут
			ld de,trd_name
			ld bc,8
			ldir ;подставить имя диска
			
			call input_name
			
			;call wait_releas
			
;WAITKEY	XOR A:IN A,(#FE):CPL:AND #1F:JR Z,WAITKEY
			
			ld a,(cur_drive) ;подготовка сообщения для подтверждения
			add a,"A"
			ld (mes_trd_dsk+3),a
			ld hl,mes_read_trd
			call print
			ld hl,mes_trd_dsk
			call print			
			;call paint_win
	
			call trd_w ;
			cp "Y"
			jp nz,start_h ;выход если нет согласия

			
read_trd	;чтение образа TRD 
			ld      a,(cur_drive) ;текущий диск
            ld      (#5d19) ,a
            ld      c,1 
            call    #3d13
            ld      c,#18
            call    #3d13
			ld de,0
			ld      (#5ceb),de ;начало сектор дорожка
		
			ld hl,trn_buf+16 ;очистим имя файла в пакете для отправки
			ld (hl),0
			ld de,trn_buf+16+1
			ld bc,file_name_len
			ldir
			
			
			;обрежем лишние пробелы в конце имени, если есть
			ld bc,7
			ld hl,trd_name+7
read_trd_chk_n
			ld a,(hl)
			cp " "
			jr nz,read_trd_chk_n2
			dec hl
			dec c
			jr nz,read_trd_chk_n
			
read_trd_chk_n2			
			ld hl,trd_name
			ld de,trn_buf+16 ;перекинем имя файла в запрос
			;ld bc,file_name_len
			inc c
			ldir
			;добавить расширение
			ex de,hl
			ld (hl),"."
			inc hl
			ld (hl),"t"
			inc hl
			ld (hl),"r"
			inc hl
			ld (hl),"d"
			
			xor a
			ld (trn_buf+12),a ;размер файла 655360
			ld (trn_buf+13),a 
			ld (trn_buf+15),a 
			ld a,#a
			ld (trn_buf+14),a

			
			ld a,4 ;Тип пакета Запрос передачи файла
			ld (trn_buf+3),a
			ld hl,0 ;счётчик частей
			ld (trn_buf+4),hl
			ld ix,640 ;частей всего цикл
read_trd3	;цикл		
			ld hl,(trn_buf+4) ;чтения части файла
			call toDecimal
		    ld hl,decimalS+2 ;
			ld de,mes_rd_part_file_num
			ld bc,3
			ldir
			ld hl,mes_rd_part_file ;сообщение
			call print_sys		

			push ix
			pop hl
			call print_progress			
			
			ld  de,(#5ceb) ;сектор дорожка
			ld hl,trn_buf+pack_size_32_com
			ld bc,#0405 ;чтение 4 сектора
			call #3d13
read_trd3_retr			
	call check_init_t ;авто проверка инициализации
			
			ld a,(23556) ;сист. переменная нажатая клавиша 
			cp " "
			jr nz,r_calc_next_pos3 ;продолжим
			ld hl,mes_stopped ;прервём
			call print_sys
			call wait_releas
			jp exit_h
			
r_calc_next_pos3	
			ld de,trn_buf
			ld bc,pack_size_1024_com+pack_size_32_com
			ld (check_sum_size),bc
			call calc_check_sum ;проверить сумму
			ld (trn_buf+pack_size_1024_com+pack_size_32_com),hl ;записать её в пакет
			
			ld hl,trn_buf ;откуда
			ld de,pack_size_1024 ;размер
			call Wifi.tcpSend ;отправить
			jr c,read_trd3_retr ;если ошибка, то новая попытка
			
			ld hl,mes_trq_part_file ;сообщение пока ждём ответ
			call print_sys
			
			ld hl,rec_buf ;куда
			ld (Wifi.buffer_pointer),hl
			call Wifi.getPacket ;приём
			jr nc,r_check_pass1 ;
			;call init_dev ;сбросить приёмник
			jr read_trd3_retr ;если ошибка, то новая попытка
			
r_check_pass1	;принято	
			ld hl,mes_trn_part_file ;сообщение
			call print_sys
;WAITKEY	XOR A:IN A,(#FE):CPL:AND #1F:JR Z,WAITKEY
			; ld a,(check_sum_on) ;временно выключим контроль суммы
			; ld (check_sum_on_tmp),a
			; xor a
			; ld (check_sum_on),a
			ld hl,rec_buf
	ld bc,pack_size_32_com
	ld (check_sum_size),bc ;задать длину пакета для подсчёта
			ld c,5 ; Тип пакета Файл
			call check_pack
			; ld (check_sum_on_tmp),a ;вернём флаг
			; ld (check_sum_on),a	
			jr nc,r_check_pass ;
			;call init_dev ;сбросить приёмник
			jr read_trd3_retr ;если ошибка, то новая попытка
			
r_check_pass	;проверка прошла		
			
			call calc_next_pos
			
			ld hl,(trn_buf+4) ;следующая честь файла
			inc hl	
			ld (trn_buf+4),hl
			
			dec ix
			ld a,ixl
			or ixh
			jp nz,read_trd3
			
			ld hl,mes_trn_file ;сообщение что передали
			call print_sys	
			;call wait_releas
			jp start_h
			
			
input_name ;ввод имени файла
			ld ix,trd_name
			;ld de,trd_name ;указатель на имя файла
input_name_			
			;очистим место под курсор
			ld hl,mes_trd_name_curs+3		
			ld (hl)," "
			ld de,mes_trd_name_curs+3+1
			ld bc,16-1
			ldir
			;нарисуем курсор
			ld hl,14+11-6  ;поле метка курсора
			push ix
			pop de
			add hl,de
			ld (hl),"^"
			ld hl,mes_trd_name ;напечатать имя
			call print
			ld a,col_win
			call paint_win		
			
			call input_key ;запросить клавишу
			ld a,c
			cp  #0d ;Enter
			ret z
			
            cp 		5
            jp      z,input_name_left
            cp 		8
            jp      z,input_name_right
            cp 		7
            jp      z,input_name_up
            cp 		6
            jp      z,input_name_down
            cp 		0
            jp      z,input_name_BS ;стереть, backspace
;input_name_sym
			;ld a,c
			cp 		" "
			jr      c,input_name_ ;если недопустимы символ, в начало
			cp 		"{"
			jr      nc,input_name_ ;если недопустимы символ, в начало	
			ld      (ix),a ; записать букву в имя
			inc ix
			push ix
			pop de
			ld hl,trd_name+ 7;проверка на пределы имени
			and a
			sbc hl,de
			jr nc,input_name_ ; в начало	
			dec ix ;иначе отмена
			jr input_name_ ; в начало

input_name_up
			ld a,255
			ld 	(23556),a
			jr input_name_
input_name_down
			ld a,255
			ld 	(23556),a
			jr input_name_
			
input_name_left
			ld a,255
			ld 	(23556),a
			dec ix
			push ix
			pop de
			ld hl,trd_name-1 ;проверка на пределы имени
			and a
			sbc hl,de
			jr c,input_name_ ; в начало	
			inc ix ;иначе отмена	
			jp input_name_
input_name_right
			ld a,255
			ld 	(23556),a
			inc ix
			push ix
			pop de
			ld hl,trd_name+ 7;проверка на пределы имени
			and a
			sbc hl,de
			jp nc,input_name_ ; в начало	
			dec ix ;иначе отмена
			jp input_name_ ; в начало
input_name_BS
			ld (ix)," "
			jr input_name_left			
			
trd_w ;ожидание согласия и выбор диска
			halt
			ld 		a,(23556) ;сист. переменная нажатая клавиша 
			cp 		"1"
            jp      z,trd_w_driveA ;1
            cp 		"A"
            jp      z,trd_w_driveA ;A			
            cp 		"2"
            jp      z,trd_w_driveB ;2
            cp 		"B"
            jp      z,trd_w_driveB ;B
            cp 		"3"
            jp      z,trd_w_driveC ;3
            cp 		"C"
            jp      z,trd_w_driveC ;C
            cp 		"4"
            jr      z,trd_w_driveD ;4
            cp 		"D"
            jr      z,trd_w_driveD ;D
			cp 		"N"
            ret z
			cp 		"Y"
            ret z ;
			jr trd_w
			
trd_w_driveA
			ld a,0
			ld (cur_drive),a
			add a,"A"
			ld (mes_trd_dsk+3),a
			ld hl,mes_trd_dsk
			call print
			jp trd_w
trd_w_driveB
			ld a,1
			ld (cur_drive),a
			add a,"A"
			ld (mes_trd_dsk+3),a
			ld hl,mes_trd_dsk
			call print
			jp trd_w
trd_w_driveC
			ld a,2
			ld (cur_drive),a
			add a,"A"
			ld (mes_trd_dsk+3),a
			ld hl,mes_trd_dsk
			call print
			jp trd_w
trd_w_driveD
			ld a,3
			ld (cur_drive),a
			add a,"A"
			ld (mes_trd_dsk+3),a
			ld hl,mes_trd_dsk
			call print
			jp trd_w				
			
	
	
			
write_trd_ ;записать TRD с сервера на диск
            ld     a,(files)
            or     a      ;no files?
            jp     z,wait
            ; ld     a,(open_trd_flag)
            ; or     a      ;нельзя скопировать, если открыт образ
            ; jp     nz,wait
			
			ld a,(cur_drive)
			add a,"A"
			ld (mes_trd_dsk+3),a
			ld hl,mes_write_trd
			call print
			ld hl,mes_trd_dsk
			call print	
			ld a,col_win
			call paint_win
	
			call trd_w
			cp "Y"
			jp nz,start_h ;выход если нет согласия
			
write_trd	;запись образа TRD на диск
			ld      a,(cur_drive) ;текущий диск
            ld      (#5d19) ,a
            ld      c,1
            call    #3d13
            ld      c,#18
            call    #3d13
			ld de,0
			ld      (#5ceb),de ;начало сектор дорожка
			ld bc,cat_r
			ld     a,(open_trd_flag)
            or     a      ;если открыт образ
			ld a,(curfile) ;текущ файл если образ не открыт
            jr     z,write_trd2
			ld bc,cat_r2 
			ld a,(curfile_r2) ;текущ файл если образ открыт
write_trd2
			ld l,a
			ld h,0
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add hl,bc
			; dec a
			; jr nz,write_trd2
;write_trd1			
			ld de,trn_buf+16 ;перекинем имя файла в запрос
			ld bc,file_name_len
			ldir
			
			xor a
			ld (trn_buf+12),a ;размер файла 655360
			ld (trn_buf+13),a 
			ld (trn_buf+15),a 
			ld a,#a
			ld (trn_buf+14),a
			
			ld a,2 ;Тип пакета Запрос приёма файла			
			ld (trn_buf+3),a
			ld hl,0 ;счётчик частей
			ld (trn_buf+4),hl
			ld ix,640 ;частей всего
write_trd3 ;цикл
	call check_init_r ;авто проверка инициализации
	
			push ix
			pop hl
			call print_progress		
			
			ld a,(23556) ;сист. переменная нажатая клавиша 
			cp " "
			jr nz,calc_next_pos3 ;продолжим
			ld hl,mes_stopped ;прервём
			call print_sys
			call wait_releas
			jp exit_h
			
calc_next_pos3			
	
			ld hl,trn_buf ;откуда
			ld de,pack_size_32_com ;размер
			call Wifi.tcpSend ;отправить
			jr c,write_trd3 ;если ошибка, то новая попытка

			ld hl,mes_req_part_file ;сообщение пока ждём ответ
			call print_sys
			
			ld hl,rec_buf ;куда
			ld (Wifi.buffer_pointer),hl
			call Wifi.getPacket ;приём
			jr nc,check_pass1
			;call init_dev
			jr write_trd3 ;если ошибка, то новая попытка
			
check_pass1	;принят пакет
			ld hl,mes_rec_part_file ;сообщение
			call print_sys
;WAITKEY	XOR A:IN A,(#FE):CPL:AND #1F:JR Z,WAITKEY
			ld hl,rec_buf
	ld bc,pack_size_1024_com+pack_size_32_com
	ld (check_sum_size),bc ;задать длину пакета для подсчёта
			ld c,3 ; Тип пакета Файл
			call check_pack
			jr nc,check_pass
			;call init_dev
			jr write_trd3 ;если ошибка, то новая попытка
			
check_pass	;проверка прошла		
			ld hl,(trn_buf+4)
			call toDecimal
		    ld hl,decimalS+2 ;
			ld de,mes_wrt_part_file_num
			ld bc,3
			ldir
			ld hl,mes_wrt_part_file ;сообщение
			call print_sys			
			
			ld  de,(#5ceb) ;сектор дорожка
			ld hl,rec_buf+32
			ld bc,#0406 ;запись 4 сектора
			call #3d13
			
			call calc_next_pos
			
			ld hl,(trn_buf+4)
			inc hl	
			ld (trn_buf+4),hl

			dec ix
			ld a,ixl
			or ixh
			jp nz,write_trd3
			
			ld hl,mes_rec_file ;сообщение что приняли
			call print_sys	
			;call wait_releas
			jp start_h
			
		
paint_win
			;покрасим окно сообщения
			ld hl,win_pos
			ld (hl),a
			ld d,h
			ld e,l
			inc de
			ld bc,win_size
			ldir
			ld hl,win_pos+32
			ld (hl),a
			ld d,h
			ld e,l
			inc de
			ld bc,win_size
			ldir
			ld hl,win_pos+32+32
			ld (hl),a
			ld d,h
			ld e,l
			inc de
			ld bc,win_size
			ldir
			ret

check_pack		
			;проверка пакета 
			;на входе в C - Тип пакета, в HL - адрес
			ld a,(hl)
			cp "Z"
			jp nz,check_pac_error
			inc hl
			ld a,(hl)
			cp "X"
			jp nz,check_pac_error
			inc hl
			ld a,(hl)
			cp "N"		
			jp nz,check_pac_error
			inc hl
			ld a,(hl)
			cp c ;Тип пакета
			jp nz,check_pac_error
			inc hl
			ld a,(hl)
			ex de,hl
			ld hl,trn_buf+4
			cp (hl) ;смещение 
			ex de,hl
			jp nz,check_pac_error
			inc hl
			ld a,(hl)
			ex de,hl
			ld hl,trn_buf+5
			cp (hl) ;смещение старший
			ex de,hl
			jr nz,check_pac_error			
		
			ld a,(check_sum_on) ;проверять или нет сумму
			or a
			jr z,check_pack_sk 
			ld de,rec_buf
			call calc_check_sum ;проверить сумму
			ex de,hl
			ld bc,rec_buf
			ld hl,(check_sum_size) ;pack_size_1024_com+pack_size_32_com
			add hl,bc
			ex de,hl
			ld a,(de)
			cp l
			jr nz,check_chs_error
			inc de
			ld a,(de)
			cp h
			jr nz,check_chs_error
check_pack_sk			
			and a ;флаг сбросим С=0 
			ret ;успешно проверен
			
check_pac_error ;проверка пакета не прошла
			ld hl,mes_pac_error
			call print_sys
			ccf ;C=1
			ret
			
			
check_chs_error
            ;проверка суммы не прошла
			ld hl,mes_chs_error
			call print_sys
			ccf
			ret

wait_releas
			halt ;ждём отпускания клавиши
			ld 	a,(23556) ;сист. переменная нажатая клавиша 
			cp #ff
			jr nz,wait_releas
			ret


calc_next_pos		;вперёд на 4 сектора	
			ld b,4
			ld  de,(#5ceb) 
calc_next_pos2		
			inc e
			ld a,e
			cp 16
			jr c,calc_next_pos1
			inc d
			ld e,0
calc_next_pos1
			djnz calc_next_pos2
			ld (#5ceb),de			
			ret
			
			
cls_pic
			ld hl,#4000
			ld de,#4001
			ld bc,6144
			ld (hl),0
			ldir
			ld (hl),col_fon
			ld bc,768-1	
			ldir
			ret
	
	
printcat_r ;печать каталога правая панель
			ld a,(panel) ;проверка надо ли скрыть курсор
			cp "R"
			ld a,colorcatc_act	
			jr z,printcat_r1
			ld a,colorcat_		
printcat_r1
			ld (colorcatc_r),a
	
            ld      hl,col_pos_r ;
            xor		a
clscat_r
            ld      d,h
            ld      e,l
            inc     de
            ld      c,a  ;keep
            ld      a,(shiftcat_r)
            ld      b,a
            ld      a,(curfile_r)
            and     a
            sbc     a,b
            cp      c
            ld      (cursor_r),a ;posit
            ld      a,(colorcat_r)
            jr      nz,clscat01_r
            ld      a,(colorcatc_r) ;mark
clscat01_r
            ld      (hl),a  ;раскраска атрибутами
            ld      a,c  ;restor
            ld      bc,9
            ldir
            ld      bc,32-9
            add     hl,bc
            inc     a
            cp      files_view
            jr      c,clscat_r

       ;     ld      hl,catposit
       ;     call    print
	   
			ld a,(files_r)
			or a
			ret z ;выход если нет файлов
			
             ;теперь печать имён файлов
            ld      a,(shiftcat_r)
			ld xl,a
            ; or      a
            ; jr      z,printcat00
			ld      l,a
			ld 		h,0
;printcat01  ;find name file
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			ld bc,cat_r ;адрес обычного каталога			
			ld a,(open_trd_flag)
			or a
			jr z,printcat00_r
			ld bc,cat_open_trd ;адрес каталога образа
printcat00_r
			add hl,bc
            ;dec    a
            ;jr    nz,printcat01
;printcat00

            xor     a
printcat02_r
            push    af
            push    hl
                       ;print 24 row

            ld      (catposit_r+1),a
            ld      hl,catposit_r ;установим позицию печати
            call    print
            pop     hl
            push    hl
			ld a,(open_trd_flag)
			or a
			jr z,printcat02_r_net
			call format_name_trd ;подготовим имя, если это открытый образ
			jr printcat02_r_skip
printcat02_r_net
			call format_name_net
printcat02_r_skip
			;отметка галкой
			ex de,hl
			ld hl,cat_mark_r
			ld a,xl
			add l
			ld l,a
			ld a,(hl)
			ex de,hl
			or a
			jr z,printcat_mark_r
			push hl
			ld a,0-5 ;знак галка
			ld bc,8
			add hl,bc
			ld (hl),a
			pop hl
printcat_mark_r			
			inc xl
			;
			
			ld bc,12 ;длина
            call    print_ ;печать одного имени файла
            ;ld      hl,catspace
            ;call    print
            pop     hl
            ld      bc,lenghtName ;на след. имя
            add     hl,bc
            pop     af
            inc     a
			ld 		bc,(files_r) ;проверка сколько всего файлов
			cp 		c
			ret		nc
            cp      files_view
            jr      c,printcat02_r
			ret
	
	
printcat_l ;печать каталога левая панель
		ld a,(cur_drive)
		cp 4
		jp nc,printcat_l_fat ;если FAT
			ld a,(panel) ;проверка надо ли скрыть курсор
			cp "L"
			ld a,colorcatc_act	
			jr z,printcat_l1
			ld a,colorcat_			
printcat_l1
			ld (colorcatc_l),a
			
            ld      hl,col_pos_l ;
            xor		a
clscat_l
            ld      d,h
            ld      e,l
            inc     de
            ld      c,a  ;keep
            ld      a,(shiftcat_l)
            ld      b,a
            ld      a,(curfile_l)
            and     a
            sbc     a,b
            cp      c
            ld      (cursor_l),a ;posit
            ld      a,(colorcat_l)
            jr      nz,clscat01_l
            ld      a,(colorcatc_l) ;mark
clscat01_l
            ld      (hl),a  ;раскраска атрибутами
            ld      a,c  ;restor
            ld      bc,9
            ldir
            ld      bc,32-9
            add     hl,bc
            inc     a
            cp      files_view
            jr      c,clscat_l

       ;     ld      hl,catposit
       ;     call    print
	   
			ld a,(files_l)
			or a
			ret z ;выход если нет файлов
			
            ld      bc,cat_l  ;теперь печать имён файлов
            ld      a,(shiftcat_l)
			ld xl,a
            ; or      a
            ; jr      z,printcat00
			ld      l,a
			ld 		h,0
;printcat01  ;find name file
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add hl,bc
            ;dec    a
            ;jr    nz,printcat01
;printcat00

            xor     a
printcat02_l
            push    af
            push    hl
                       ;print 24 row

            ld      (catposit_l+1),a
            ld      hl,catposit_l ;установим позицию печати
            call    print
            pop     hl
            push    hl
			call 	format_name_trd ;подготовим
			;отметка галкой
			ex de,hl
			ld hl,cat_mark_l
			ld a,xl
			add l
			ld l,a
			ld a,(hl)
			ex de,hl
			or a
			jr z,printcat_mark_l
			push hl
			ld a,0-5 ;знак галка
			ld bc,8
			add hl,bc
			ld (hl),a
			pop hl
printcat_mark_l			
			inc xl
			;
			
			ld bc,12 ;длина
            call    print_ ;печать одного имени файла
            ;ld      hl,catspace
            ;call    print
            pop     hl
            ld      bc,lenghtName ;на след. имя
            add     hl,bc
            pop     af
            inc     a
			ld 		bc,(files_l) ;проверка сколько всего файлов
			cp 		c
			ret		nc
            cp      files_view ;проверка сколько файлов показывать
            jr      c,printcat02_l
			ret	
	
	
	
format_name_trd ;подготавливает имя файла для печати в каталоге, на выходе адрес в HL
            ld      de,catFName ;здесь временное имя файла
            ld      bc,8   ;name 8 sym
            ldir
			ld		a," " ;добавим пробел перед расширением
			ld		(de),a	
			inc		de
			ld 		a,(hl) ;если это строчная буква, то расширение 3 символа
			ldi			;перенесём первую букву расширения
			cp 		"a"
			jr 		c,format_name_trd05
			ld 		a,(hl)		;перенесём ещё две буквы
			or		a
			jr		nz,format_name_trd021
			ld		a," " ;если второй нуль, то забъём пробелами
			ld		(de),a
			inc		hl
			inc 	de
			ld 		(de),a
			inc 	hl
			inc		de
			jr		format_name_trd06
format_name_trd021			
			ldi		;перенесли вторую букву
			ld 		a,(hl)	
			or		a
			jr		nz,format_name_trd022
			ld		a," " ;если третий нуль, добъём пробелом
format_name_trd022
			ld		(de),a ;перенесли третью
			inc		hl
			inc 	de			
			jr		format_name_trd06
format_name_trd05 ;забъём пробелами если расширение одна буква
			inc		hl
			inc		hl
			ld		a," "
			ld 		(de),a
			inc 	de
			ld		(de),a
			inc 	de
format_name_trd06
            xor		a
            ld      (de),a ;marker end line
			ld 		hl,catFName
			ret	
	

;для каталога сервера
format_name_net ;подготавливает имя файла для печати в каталоге, на выходе адрес в HL
			push hl
            ld      hl,catFName ;здесь временное имя файла
			ld      de,catFName+1
			ld (hl)," "
			ld bc,8+3-1
			ldir ;почистить
			pop hl
			ld      de,catFName ;здесь временное имя файла
            ld      bc,#08ff   ;name 8 sym
format_name_net_cl
			ld a,(hl)
			cp "."
			jr z,format_name_net_ext
			ldi
			djnz format_name_net_cl
format_name_net_ext
			inc hl
			ld      de,catFName+9
            ld      bc,3   ;ext 3 sym
			ldir
			xor		a
            ld      (de),a ;marker end line
			ld 		hl,catFName
			ret	


format_name_fat_l ;подготавливает имя файла для печати в каталоге, на выходе адрес в HL
            ld      de,catFName ;здесь временное имя файла
            ld      bc,8   ;name 8 sym
            ldir
			ld		a," " ;добавим пробел перед расширением
			ld		(de),a	
			inc		de
			
			;ld bc,#0b-8
			;push hl
			;add hl,bc
			;ld a,(hl)
			;pop hl
			; bit 4,a ;признак каталога
			; jr z,format_name_fat_l_no_dir
			; ;если папка
			; ld a,"<"
			; ld (de),a
			; inc de
			; ld a,"."
			; ld (de),a
			; inc de
			; ld a,">"
			; ld (de),a
			; inc de
			; jr format_name_fat_l_ex
format_name_fat_l_no_dir	
			ld bc,3
			ldir		;перенесём расширение
format_name_fat_l_ex
			xor		a
            ld      (de),a ;marker end line
			ld 		hl,catFName
			ret	

			
	
toDecimal		;конвертирует 2 байта в 5 десятичных цифр
				;на входе в HL число
			ld de,10000 ;десятки тысяч
			ld a,255
toDecimal10k			
			and a
			sbc hl,de
			inc a
			jr nc,toDecimal10k
			add hl,de
			add a,48
			ld (decimalS),a
			ld de,1000 ;тысячи
			ld a,255
toDecimal1k			
			and a
			sbc hl,de
			inc a
			jr nc,toDecimal1k
			add hl,de
			add a,48
			ld (decimalS+1),a
			ld de,100 ;сотни
			ld a,255
toDecimal01k			
			and a
			sbc hl,de
			inc a
			jr nc,toDecimal01k
			add hl,de
			add a,48
			ld (decimalS+2),a
			ld de,10 ;десятки
			ld a,255
toDecimal001k			
			and a
			sbc hl,de
			inc a
			jr nc,toDecimal001k
			add hl,de
			add a,48
			ld (decimalS+3),a
			ld de,1 ;единицы
			ld a,255
toDecimal0001k			
			and a
			sbc hl,de
			inc a
			jr nc,toDecimal0001k
			add hl,de
			add a,48
			ld (decimalS+4),a		
			
			ret
	
decimalS	ds 5 ;десятичные цифры	


calc_check_sum ;подсчёт контрольной суммы
	ld hl,0
	;ld de,rec_buf
	ld bc,(check_sum_size) ;pack_size_1024_com+pack_size_32_com
calc_check_sum1
	push bc
	ex de,hl
	ld c,(hl)
	ld b,0
	ex de,hl
	add hl,bc
	inc de
	pop bc
	dec bc
	ld a,b
	or c
	jr nz,calc_check_sum1
	ret
	
readcat ;чтение каталога
			ld 		a,(cur_drive)
			cp 4
			jp nc,readcat_fat ;начиная с 4 будет диск FAT
            ld      c,1 ;настройка на диск
            call    #3d13
            ld      c,#18
            call    #3d13
            ld      hl,cat_l ;to
            ld      de,#0000 ;0 trk 0 sec
            ld      b,9      ;9 sect
            ld      c,5 ;read
            call    #3d13
            ld      hl,cat_l+2048+#e4;
            ld      a,(hl) ;total files
			ld      (files_l),a ;всего файлов на диске
            ;or      a
            ;ret     z  ;exit if no fales

            ; ld      c,a
            ; ld      a,128
            ; sub     c
            ; ld      (catend),a ;fill end cat
            ;ld      a,c
            ;call    markdrive
            ret
	
change_panel ;смена панели
	ld a,(panel)
	cp "L"
	jr z,change_panel_r
	call sel_left_pan
    jp     wait	
change_panel_r
	call sel_right_pan
    jp     wait
	
printcat ;печать активного каталога
	ld a,(panel)
	cp "L"
	jp z,printcat_l
	jp printcat_r
	
sel_right_pan ;выбор правой панели
	ld     a,(files_r)
	ld     (files),a
	ld     a,(shiftcat)
	ld     (shiftcat_l),a
	ld     a,(curfile)
	ld     (curfile_l),a
	ld     a,(shiftcat_r)
	ld     (shiftcat),a
	ld     a,(curfile_r)
	ld     (curfile),a
	ld a,"R"
	ld (panel),a	
	call printcat_l
	call printcat_r
	ret
sel_left_pan ;выбор левой панели
	ld     a,(files_l)
	ld     (files),a
	ld     a,(shiftcat)
	ld     (shiftcat_r),a
	ld     a,(curfile)
	ld     (curfile_r),a
	ld     a,(shiftcat_l)
	ld     (shiftcat),a
	ld     a,(curfile_l)
	ld     (curfile),a
	ld a,"L"
	ld (panel),a
	call printcat_l
	call printcat_r
	ret

init_dev ;инициализация устройства передачи
;WAITKEY	XOR A:IN A,(#FE):CPL:AND #1F:JR Z,WAITKEY
	ld hl,mes_req_init
	call print_sys
	call Uart.init

	; ld de,pack_size_1024 ;размер полного пакета
	; call start_rcv ;всё примем
	;call read_buf
	
	; ld hl,cmd_brk ;прервать передачу
	; ld de,cmd_brk_end-cmd_brk
	; call start_trn1 ;оправка	
	; call read_buf
	
	ld hl,cmd_ipclose ;закрыть соединение 
	ld de,cmd_ipclose_e-cmd_ipclose
	call start_trn1 ;оправка	
	
	ld de,pack_size_1024_full ;размер полного пакета
	call start_rcv ;всё примем
	;call read_buf
	
	;подготовить строку открытия соединения
	ld hl,cmd_ipstart 
	ld bc,cmd_ipstart_e-cmd_ipstart
	ld ix,bc ;длина
	ld de,cipstart_line
	ldir
	;имя сервера
	ld hl,server_name
init_dev_server
	ld a,(hl)
	cp " "
	jr c,init_dev1
	ldi
	inc ix
	jr init_dev_server
init_dev1
	ld a,'"'
	ld (de),a
	inc de
	inc ix
	ld a,','
	ld (de),a	
	inc de
	inc ix
	;порт 
	ld hl,server_port
init_dev_server2
	ld a,(hl)
	cp " "
	jr c,init_dev2
	ldi
	inc ix
	jr init_dev_server2
init_dev2	
	ld a,13
	ld (de),a
	inc de
	inc ix
	ld a,10
	ld (de),a
	inc ix	
	
	ld hl,cipstart_line
	ld de,ix ;длина
	call start_trn1 ;оправка

	ld de,pack_size_1024_full ;размер полного пакета
	call start_rcv ;всё примем	
	;call read_buf
	ret
	
;read_buf	;чтение в буфер
	; ld bc,257 ;очистить буфер - убрать!
	; ld hl,rec_buf
	; ld de,rec_buf+1
	; ld (hl),0
	; ldir
	
	; ld b,0 ;чтение данных
	; ld hl,rec_buf
; read_buf1
	; call Uart.read
	; ret nc
	; ld (hl),a
	; inc hl
	; djnz read_buf1
	; ret

	; ;проверка заголовка UDP
; check_id
	; ld hl,rec_buf
	; ld de,pack_id ;образец для сравнения
	; ld b,0 ;ограничение на поиск
	; ld c,pack_id_e-pack_id ;символов для сравнения
; compar1
	; ld a,(de)
	; cp (hl)
	; jr z,compar2
	; ld c,pack_id_e-pack_id
	; ld de,pack_id	;с начала образца
	; inc hl
	; djnz compar1
	; jr compar_no
; compar2
	; inc de
	; inc hl
	; dec c
	; jr nz,compar1
	; ;и в конце найти ещё ":"
; compar3
	; ld a,(hl)
	; inc hl
	; cp ":"
	; jr z,compar_ok
	; djnz compar3
; compar_no ;не нашли
	; xor a
	; ret ;
; compar_ok ;нашли
	; ;в hl указатель на начало ответного пакета
	; ld (rec_buf_adr_pack),hl
	; ex de,hl
	; ld hl,pack_size_32_com
	; add hl,de
	; ld (rec_buf_adr_pack1),hl
	; ex de,hl
	; scf
	; ret


print_progress ;печать текущего прогресса
			call toDecimal
		    ld hl,decimalS ;
			ld de,mes_progress+3
			ld bc,5
			ldir
			ld hl,mes_progress 
			call print_sys	
			ret
	
print_progress_tot ;печать общего прогресса
			call toDecimal
		    ld hl,decimalS ;
			ld de,mes_progress_tot+3
			ld bc,5
			ldir
			ld hl,mes_progress_tot 
			call print_sys	
			ret


	
markF ;отметка файлов для копирования
	ld a,(files) ;всего
	or a
	jp z,wait_cont	;если 0
	
	ld a,(panel)
	cp "R"
	jp nz,markF_l ;если левая панель
	;правая панель
	ld a,(open_trd_flag)
	or a
	jp z,markF_r1 ;если не открыт образ	
	;правая панель
	ld a,(curfile) ;номер файла
	or a
	jp z,wait_cont	;многоточие нельзя выбрать	
markF_r1
	ld a,(curfile) ;номер файла
	ld hl,cat_mark_r
	ld b,0
	ld c,a
	add hl,bc
	ld a,(hl)
	xor 1 ;меняем на противоположное
	ld (hl),a
	;call wait_releas
	jp wait_cont	

markF_l ;левая панель
	ld a,(cur_drive)
	cp 4
	jr c,markF_l_TRD ;
	ld a,(curfile) ;номер файла
	;если FAT
	call calc_cat_fat_deskr
	ld a,(ix+#0b)
	bit 4,a
	jp nz,wait_cont ;если папка, то нельзя пометить
markF_l_TRD
	ld a,(curfile)
	ld hl,cat_mark_l
	ld b,0
	ld c,a
	add hl,bc
	ld a,(hl)
	xor 1 ;меняем на противоположное
	ld (hl),a
	;call wait_releas
	jp wait_cont
	


mark_all ;отметить все файлы
	ld a,(panel)
	cp "R"
	jp nz,mark_all_l ;если левая панель
	
	;правая панель	
	ld a,(open_trd_flag)
	or a
	jp z,mark_all_r_fat ;не если не открыт образ	
	ld a,(files) ;всего
	cp 2
	jp c,wait_cont	;если только точки
	ld hl,cat_mark_r+1
	cp 2 ;если один
	jr nz,mark_all_r
	;если 1 файл
	ld (hl),1 ;отметить второй
	jp z,wait_cont	
mark_all_r
	sub 2 
	ld b,0
	ld c,a
	ld de,cat_mark_r+2
	ld (hl),1 ;отметить много
	ldir
	jp wait_cont
mark_all_r_fat
	;правая fat
	ld a,(files) ;всего
	ld hl,cat_mark_r
	cp 2 ;если один
	jr nc,mark_all_r_fat1
	;если 1 файл
	ld (hl),1 
	jp wait_cont	
mark_all_r_fat1
	sub 1 
	ld b,0
	ld c,a
	ld de,cat_mark_r+1
	ld (hl),1 ;отметить много
	ldir
	jp wait_cont	



mark_all_l ;левая панель
	ld a,(files) ;всего
	or a
	jp z,wait_cont	;если 0	
	
	ld a,(cur_drive)
	cp 4
	jr c,mark_all_l_TRD ;
	
	;если FAT	
	ld a,(files) ;всего
	cp 1
	jr nz,mark_all_l1_fat
	ld a,(curfile) ;номер файла
	call calc_cat_fat_deskr
	ld a,(ix+#0b)
	bit 4,a
	jp nz,wait_cont ;если папка, то нельзя пометить	
	ld (hl),1 ;отметить первый
	jp wait_cont
	
mark_all_l1_fat	
	ld e,a ;счётчик
	ld d,0
	ld hl,cat_mark_l
mark_all_l1_fat_cl ;цикл	
	ld a,d
	call calc_cat_fat_deskr
	ld a,(ix+#0b)
	bit 4,a
	jp nz,mark_all_l1_fat_cl1 ;если папка, то нельзя пометить
	ld (hl),1 ;отметить много		
mark_all_l1_fat_cl1
	inc hl
	inc d
	dec e
	jr nz,mark_all_l1_fat_cl
	jp wait_cont

	
	
	;если TRD
mark_all_l_TRD	
	ld a,(files) ;всего
	ld hl,cat_mark_l
	cp 1
	jr nz,mark_all_l1
	ld (hl),1 ;отметить первый
	jp z,wait_cont
mark_all_l1
	dec a
	ld b,0
	ld c,a
	ld de,cat_mark_l+1
	ld (hl),1 ;отметить много
	ldir
	jp wait_cont
	
	

unmark_all ;снять отметку со всех файлов
	ld a,(panel)
	cp "R"
	jp nz,unmark_all_l ;если левая панель
	;правая
	ld hl,cat_mark_r	
	ld de,cat_mark_r+1
	ld (hl),0
	ld bc,128-1
	ldir
	jp wait_cont	
unmark_all_l ;левая
	ld hl,cat_mark_l	
	ld de,cat_mark_l+1
	ld (hl),0
	ld bc,128-1
	ldir
	jp wait_cont

;pack_size dw 0 ;размер пакета	
check_sum_on db 1; флаг включения проверки контрольной суммы	
check_sum_on_tmp db 1; сохранение флага	
colorcatc_l db colorcatc_act ;цвет выбранного файла слева
colorcat_l db  colorcat_; цвет окна каталога слева
colorcatc_r db colorcatc_act ;цвет выбранного файла справа
colorcat_r db  colorcat_; цвет окна каталога	справа
cursor_l   db     0; строка курсора слева
cursor_r   db     0; строка курсора справа
cur_drive db 0 ; дисковод
shiftcat db 0;сдвиг по каталогу
shiftcat_r db 0;сдвиг по каталогу правая панель
shiftcat_l db 0;сдвиг по каталогу левая панель
keyRepeat db 20 ;пауза перед повтором нажатия клавиши
keyLast db 255; последняя нажатая клавиша
keyDelayF db 0 ;флаг что уже была пауза нажатия
files db 0 ;всего файлов
curfile db 0 ; текущий файл
files_r db 0 ;всего файлов правая панель
curfile_r db 0 ; текущий файл правая панель
files_l db 0 ;всего файлов левая панель
curfile_l db 0 ; текущий файл левая панель
;col_pos dw #5800 ;позиция для покраски
panel db 0; текущая панель
catFName  ds		13 ;имя файла для каталога
		 db		0 ;маркер конца имени
open_trd_flag db 0 ;открыт образ
;rec_buf_adr_pack dw 0 ;адрес начала принятого пакета без заголовка UDP
;rec_buf_adr_pack1 dw 0 ;адрес начала принятого пакета без заголовка ZXN
cur_cat_pag dw 0 ;текущая страница каталога сервера
;rec_buf_adr_pack1_tmp dw 0 ;временное хранилище
curfile_r2 db 0 ;хранение позиции курсора при открытии образа
shiftcat_r2 db 0
files_r2 db 0
files2 db 0
check_init_val dw 0 ;контрольная сумма для системы внеочередной инициализации
check_init_count db 0 ;счётчик системы 
cur_trk dw 0 ;текущие сектор-дорожка
copyF_RL_source_file dw 0 ;указатель на исходный файл для копирования
copyF_RL_source_sec_size dw 0 ;размер в секторах исходного файла
copyF_RL_source_trk dw 0 ;исходные дорожка-сектор файла
copyF_RL_dest_sec_cat db 0 ;сектор каталога где запись о файле назначения
copyF_FAT_RL_last_part dw 0 ;последний кусок файла для записи
;copyF_RL_first_pack_flag db 0; флаг для первого пакета файла
check_sum_size dw 0 ;длина области для подсчёта суммы
cat_mark_cur dw 0 ;текущая позиция в таблице отмеченных файлов слева
cat_mark_cur_cl db 0 ;позиция цикла копирования слева
cat_mark_cur_tot db 0 ;осталось копировать файлов

catposit_l ;позиция печати каталога слева
	db 22,0,0,0
catposit_r ;позиция печати каталога справа
	db 22,0,30,0
cat_space db "            ",0 ;пробелы для очистки 
mes_sys_at ;позиция печати сист. сообщений
	db 22,23,0,0
mes_req_cat
	db "Cat in request  ",0
mes_rec_cat
	db "Cat "
mes_rec_cat_num	
	db "000 in OK   ",0
mes_req_part_file
	db "File in request ",0	
mes_trq_part_file
	db "File out request",0	
mes_rec_file
	db "File in OK      ",0
mes_trn_file
	db "File out OK     ",0	
mes_rec_part_file
	db "Part in OK      ",0
mes_trn_part_file
	db "Part out OK     ",0
mes_wrt_part_file
	db "Write "
mes_wrt_part_file_num	
	db "000       ",0	
mes_rd_part_file
	db "Read "
mes_rd_part_file_num	
	db "000        ",0	
mes_req_init
	db "Init            ",0
; mes_translink_off
	; db "ESP link off    ",0
; mes_translink_on
	; db "ESP link on     ",0
mes_esp_rst
	db "ESP reset       ",0
mes_ok
	db "OK              ",0

mes_win_cls
	db 22,14,13,"                "
	db 22,15,13,"                "
	db 22,16,13,"                ",0
mes_trd_name
	db 22,15,13,"Name: ";
trd_name db "          "	
mes_trd_name_curs
	db 22,16,13,"                ",0

mes_read_trd
	db 22,14,13," READ TRD, dsk:",0;
mes_write_trd
	db 22,14,13,"WRITE TRD, dsk:",0;
mes_trd_dsk db 22,14,28,"A"
	db 22,15,13,"  Y to start   "
	db 22,16,13,"  N to cancel  ",0
mes_chs_error
	db "Check sum error ",0		
mes_pac_error
	db "Packet error    ",0	
mes_stopped
	db "Stopped         ",0
mes_title
	db 22,0,14,"AY232K v0.1.2"	
	db 22,2,14,"Arrow keys"
	db 22,3,14,"R - renew"
	db 22,4,14,"A-D, H - disk"
	db 22,5,14,"En - run/open"
	db 22,6,14,"5 - copy"	
	db 22,7,14,"T - TRD copy"
	db 22,8,14,"Ext - panel"
	db 22,9,14,"Sp - mark/stop"
	db 22,10,14,"K - mark all"
	db 22,11,14,"J - unmark all"
	db 22,12,14,"E - exit"
	;db 22,11,15,"S - Sum: "
;mes_check_sum
	;db "Y"
	; db 22,13,15,"Dangerous(!):"
	db 22,13,14,"I - ESP Reset"

	db 0
mes_wait	
	db 22,17,18,"Wait..",0
mes_wait_off
	db 22,17,18,"      ",0
mes_copy_file
	db 22,14,13,         "COPY FILE, dsk:",0;
mes_copy_error 
	db 22,15,13,16,2*8+7,"Error copy file "
	db 22,16,13,         "                ",0

mes_progress	
	db 22,19,18,"00000",0	
mes_progress_off
	db 22,19,18,"     ",0	
	
mes_progress_tot	
	db 22,20,18,"00000",0	
mes_progress_tot_off
	db 22,20,18,"     ",0

mes_curent_drive	db 22,0,0,"Select ("
mes_curent_drive_letter db " )",0
mes_fdd	db 22,1,0,"A-D - FDD",0
mes_fat_found db 22,2,0,0
mes_fat_not_found db 22,2,0,"FAT not found",13,0
mes_next_line db 13,0

mes_read_cfg_err db "No "
file_cfg_name db "AY232K_iC",0

mes_server
	db 22,22,18,"Server: "
server_name ds 15 ;имя сервера
	db 0
mes_port
	db 22,23,18,"Port: "
server_port ds 4 ;порт	
	db 0



scroll_sys ;скрол области системных сообщений
	; ld de,#4000+2048+2048+3*32
	; ld hl,#4000+2048+2048+4*32
	; ld a,8
; scroll_sys1
	; push hl
	; push de
	; ld bc,scroll_len ; длина области для сдвига
	; ldir
	; pop de
	; pop hl
	; inc h
	; inc d
	; dec a
	; jr nz,scroll_sys1
	
	; ld de,#4000+2048+2048+4*32
	; ld hl,#4000+2048+2048+5*32
	; ld a,8
; scroll_sys2
	; push hl
	; push de
	; ld bc,scroll_len ; длина области для сдвига
	; ldir
	; pop de
	; pop hl
	; inc h
	; inc d
	; dec a
	; jr nz,scroll_sys2
	
	ld de,#4000+2048+2048+5*32
	ld hl,#4000+2048+2048+6*32
	ld a,8
scroll_sys3
	push hl
	push de
	dup scroll_len ; длина области для сдвига
	ldi
	edup
	pop de
	pop hl
	inc h
	inc d
	dec a
	jr nz,scroll_sys3
	
	ld de,#4000+2048+2048+6*32
	ld hl,#4000+2048+2048+7*32
	ld a,8
scroll_sys4
	push hl
	push de
	dup scroll_len ; длина области для сдвига
	ldi
	edup
	pop de
	pop hl
	inc h
	inc d
	dec a
	jr nz,scroll_sys4
	ret
	
	
print_sys
	push hl
	call scroll_sys
	ld hl,mes_sys_at
	call print ;установка позиции
	pop hl
	call print
	ret
;печать до символа 0
;hl - text address
;13-enter
;16-color(атрибуты 128+64+pap*8+ink)
;20-inverse
;21-отступ от левого края
;22-at
print_  ;var 2: print text lenght in bc
        ld      a,(hl)
        call    prsym
        inc     hl
        dec     bc
        ld      a,b
        or      c
        jr      nz,print_
        ret
aupr    pop     hl
        call    print
        push    hl
        ret
;start print to 0
print   ld      a,(hl)
        inc     hl
        or      a
        ret     z
        cp      23
        jr      c,prin
        call    prsym
        jr      print
prin
        cp      13
        jr      nz,prin0
        ld      a,(space)
        ld      (xtxt),a
        ld      a,(ytxt)
        inc     a
        cp      23
        jr      c,pr13_0
        xor     a
pr13_0  ld      (ytxt),a
        jr      print
prin0   cp      16
        jr      nz,prin1
        ld      a,(hl)
        inc     hl
        ld      (23695),a
        jr      print
prin1   cp      20
        jr      nz,prin2
        ld      a,(hl)
        inc     hl
        or      a
        jr      z,pr20_0
        ld      a,#2f
        ld      (pr0),a
        ld      (pr1),a
        ld      (pr2),a
        ld      (pr3),a
        jr      print
pr20_0  ld      (pr0),a
        ld      (pr1),a
        ld      (pr2),a
        ld      (pr3),a
        jr      print
prin2   cp      22
        jr      nz,prin3
        ld      a,(hl)
        ld      (ytxt),a
        inc     hl
        ld      a,(hl)
        ld      (xtxt),a
        inc     hl
        jr      print
prin3   cp      21
        jr      nz,print
        ld      a,(hl)
        inc     hl
        ld      (space),a
        jr      print
prsym
        push    af
        push    bc
        push    de
        push    hl
        push    ix
        ld      de,(ytxt)
        inc     d
        ld      (ytxt),de
        dec     d
        ex      af,af'
        ld      a,d
        cp      41
        jr      c,prs
        ld      a,e
        inc     a
        cp      24
        jr      c,prs1
        xor     a
prs1    ld      (ytxt),a
        ld      a,(space)
        ld      (xtxt),a
prs     ex      af,af'
        ld      l,a
        ld      h,#00
        add     hl,hl
        add     hl,hl
        add     hl,hl
        ld      bc,font
        add     hl,bc
        push    hl
        ld      a,d
        add     a,a
        ld      d,a
        add     a,a
        add     a,d
        add     a,#02
        ld      d,a
        and     #07
        ex      af,af'
        ld      a,d
        rrca
        rrca
        rrca
        and     #1F
        ld      d,a
        ld      a,e
        and     #18
        add     a,#40
        ld      h,a
        ld      a,e
        and     #07
        rrca
        rrca
        rrca
        add     a,d
        ld      l,a
        ld      (posit),hl
        pop     de
        ld      b,#08
        ex      af,af'
        jr      z,L73C7
        ld      xh,b
        cp      #02
        jr      z,L73D6
        cp      #04
        jr      z,L73E9
L73A7   ld      a,(hl)
        rrca
        rrca
        ld      b,a
        inc     hl
        ld      a,(hl)
        and     #0F
        ld      c,a
        ld      a,(de)
pr0     nop
        and     #FC
        sla     a
        rl      b
        sla     a
        rl      b
        or      c
        ld      (hl),a
        dec     hl
        ld      (hl),b
        inc     h
        inc     de
        dec     xh
        jr      nz,L73A7
        jr      prsc1
L73C7   ld      a,(hl)
        and     #03
        ld      c,a
        ld      a,(de)
pr1     nop
        and     #FC
        or      c
        ld      (hl),a
        inc     h
        inc     de
        djnz    L73C7
        jr      prsc
L73D6   ld      a,(hl)
        and     #C0
        ld      b,a
        ld      a,(de)
pr2     nop
        and     #FC
        rrca
        rrca
        or      b
        ld      (hl),a
        inc     h
        inc     de
        dec     xh
        jr      nz,L73D6
        jr      prsc
L73E9   ld      a,(hl)
        rrca
        rrca
        rrca
        rrca
        ld      b,a
        inc     hl
        ld      a,(hl)
        and     #3F
        ld      c,a
        ld      a,(de)
pr3     nop
        and     #FC
        sla     a
        rl      b
        sla     a
        rl      b
        sla     a
        rl      b
        sla     a
        rl      b
        or      c
        ld      (hl),a
        dec     hl
        ld      (hl),b
        inc     h
        inc     de
        dec     xh
        jr      nz,L73E9
        jr      prsc1
prsc    ld      hl,(posit)
        ld      a,h
        and     #18
        rrca
        rrca
        rrca
        add     a,#58
        ld      h,a
        ld      a,(23695)
        ;ld      (hl),a ;отключена раскраска
        jr      prse
prsc1   ld      hl,(posit)
        ld      a,h
        and     #18
        rrca
        rrca
        rrca
        add     a,#58
        ld      h,a
        ld      a,(23695)
        ;ld      (hl),a
        inc     hl
        ;ld      (hl),a
prse    pop     ix
        pop     hl
        pop     de
        pop     bc
        pop     af
        ret
posit   dw      0
space   nop
ytxt    nop
xtxt    nop


;Приём пакета
start_rcv
		push de
		ld hl,mes_wait
		call print
		pop de
		ld	hl,rec_buf ; куда принимать
		;ld	de,pack_size_1024_full ;размер 
start_rcv1		
		call Uart.read
		jr nc,start_rcv_ex
		ld (hl),a
		inc hl
		dec de 
		ld a,d 
		or e 
		jr nz,start_rcv1 
		
		ld hl,mes_wait_off
		call print
		scf ;флаг C=1, значит приём успешный
		ret	
start_rcv_ex
		ld hl,mes_wait_off
		call print
		xor a ;C=0, если не успешно
		ret


;Передача пакета
start_trn
		ld hl,mes_wait
		call print

		ld	hl,trn_buf ;откуда
		;ld	de,(pack_size)
start_trn1
		ld a,(hl)
		call Uart.write
		inc hl 
		dec de
		ld a,d
		or e
		jr nz,start_trn1
		
		ld hl,mes_wait_off
		call print
		ret	
		
check_init_r ;при приёме
	ld hl,trn_buf+3
	;ld hl,trn_buf+pack_size_32_com
	jr check_init
check_init_t ;при передаче
	ld hl,trn_buf+3
	;ld hl,trn_buf+pack_size_1024_com+pack_size_32_com
check_init
	;проверка не пора ли сбросить устройство
	;если слишком часто передаётся один и тот же пакет
	;значит нужно снова инициализировать
	ld de,(check_init_val)
	ld a,(hl)
	cp e
	jr nz,check_init1
	inc hl
	ld a,(hl)
	dec hl
	cp d
	jr nz,check_init1
	;если суммы совпадают
	ld a,(check_init_count) ;счётчик попыток
	inc a
	ld (check_init_count),a
	cp check_init_max ;сколько попыток максимум
	ret c
	xor a
	ld (check_init_count),a	
	call init_dev ;пора сбросить
	ret
check_init1 ;не равны
	ld e,(hl) ;запомнить новое значение
	inc hl
	ld d,(hl)
	ld (check_init_val),de	
	xor a ;сбросить счётчик
	ld (check_init_count),a
	ret

;здесь драйвер устройства
		include "drivers/zx-wifi.asm"
		include "drivers/wifi.asm"
		include "at_command.asm"		



	; ;org #6450 ;обработчик прерывания
; interrupt
	; ; push af
	; ; push bc
	; ; push de
	; push hl
	; ; exx
	; ; ex af,af'
	; ; push af
	; ; push bc
	; ; push de
	; ; push hl
	; ; push ix
	; ; push iy
	; ; call #6500
	; ld hl,frame
	; inc (hl)
	; ; pop iy
	; ; pop ix
	; ; pop hl
	; ; pop de
	; ; pop bc
	; ; pop af
	; ; ex af,af'
	; ; exx
	; pop hl
	; ; pop de
	; ; pop bc
	; ; pop af
	; rst #38 ;вместо прерывания im 1
	; ei
	; ret
; frame dw 0
		; align 256
; interrupt_vec equ $-1 ;указатель на обработчик прерываний
	; dw 0


read_cfg ;прочитать файл конфигурации
	ld hl,file_cfg_name
	ld      c,#13 ;move file info to syst var
    call    #3d13
	ld      a,c
	cp 		#ff
	jr 		z,read_cfg_err
    ld      c,#0a ;find file
    call    #3d13
    ld      a,c
	cp 		#ff
	jr 		z,read_cfg_err ;если не нашли файла
    ld      c,#08 ;read file title
    call    #3d13
	ld      a,c
	cp 		#ff
	jr 		z,read_cfg_err
    ld      hl,file_cfg_name_buf ;куда
    ld      de,(#5ceb) ;начало файла сектор дорожка
    ld      bc,#0105 ;считать один сектор
    call    #3d13
	ld      a,c
	cp 		#ff
	jr 		z,read_cfg_err	
	
	;распознать файл настроек
	ld  hl,file_cfg_name_buf ;откуда
	ld 	de,server_name ;куда
	ld bc,#0fff ;максимум имя сервера 15 символов	
read_cfg_cl	
	ld a,(hl)
	cp " "
	jr c,read_cfg_cl_skip
	;тут нашли имя сервера
	ldi
	djnz read_cfg_cl
	

read_cfg_cl_skip
	;пропустить конец строки
	ld bc,#0fff ;максимум имя сервера15 символов	
read_cfg_cl1
	ld a,(hl)
	cp " "
	jr nc,read_cfg_cl_skip2
	;тут нашли имя сервера
	inc hl
	djnz read_cfg_cl1

read_cfg_cl_skip2
	;порт
	ld 	de,server_port ;куда	
	ld bc,#04ff ;максимум длина порта 4
	;ld ixl,0 ;счётчик
read_cfg_cl2
	ld a,(hl)
	cp " "
	jr c,read_cfg_cl_skip3
	ldi
	djnz read_cfg_cl2

read_cfg_cl_skip3	
	;настройки определения HDD
;пропустить конец строки
	ld bc,#05ff ;максимум 5	
read_cfg_cl3
	ld a,(hl)
	cp " "
	jr nc,read_cfg_cl_skip4
	;тут нашли имя сервера
	inc hl
	djnz read_cfg_cl3

read_cfg_cl_skip4	
	ld bc,#08ff ;максимум 8
	ld e,0
read_cfg_cl4
	ld a,(hl)
	sub "0"
	rr a
	rl e
	inc hl
	djnz read_cfg_cl4
	ld a,e
	ld (cng_19+1),a
	
	ret
	
	
read_cfg_err
	ld hl,mes_read_cfg_err
	call print_sys
	ret

	
init_hdd ;
;инициализация файловой системы FAT
	;xor a
	call navGetNumDrives ;узнаем какая буква последняя, сколько разделов FAT
	ld	iy,#5C3A	
	cp 5 ;если только дисководы
	jr nc,init_fs_fat_prn2
init_hdd_err
	ld hl,mes_fat_not_found
	call print	
	call wait_key
	;scf ;ошибка
	ret
	
	
init_fs_fat_prn2	
	;печать букв дисководов
	ld a,(cur_drive)
	add a,"A"
	ld (mes_curent_drive_letter),a
	ld hl,mes_curent_drive ;
	call print
	ld hl,mes_fdd ;
	call print
	;печать списка разделов
	ld hl,mes_fat_found
	call print
	ld hl,typeDrive
	ld a,(numDrives)
	cp 5
	jr c,init_fs_select_
	sub 4 ;убрать дисководы
	ld b,a ;цикл по количеству разделов
	ld c,"E" ;первая буква диска FAT
init_fs_fat_prn1
	push bc
	push hl
	ld a,c ;имя диска
	call prsym
	ld a,":" ;
	call prsym
	;тип S или H
	ld a,"H"
	pop hl
	push hl
	bit 5,(hl) ;SD?
	jr z,init_fs_fat_prn3
	bit 4,(hl) ;SD?
	jr z,init_fs_fat_prn3
	ld a,"S"	
init_fs_fat_prn3

	call prsym ;первая буква
	ld a,"D"	
	call prsym ;вторая буква
	;номер диска
	pop hl
	push hl
	ld a,(hl)
	and %00000100
	rrca
	rrca
	add "0"
	call prsym ;номер диска
	;номер раздела
	ld a,","
	call prsym ;разделитель
	pop hl
	push hl
	ld a,(hl)
	and %00000011
	add "0"
	call prsym ;номер раздела
	ld hl,mes_next_line ;новая строка
	call print
	pop hl
	inc hl
	pop bc
	inc c
	djnz init_fs_fat_prn1

	;ждём выбора буквы
init_fs_select_
	ld a,(numDrives)
	add "A"
	ld (numDrives_last),a ;последняя свободная буква 
	ld hl,numDrives_last
init_fs_select
	call input_key
	cp "A"
	jr c,init_fs_select ;не в диапазоне
	
	cp (hl)
	jr nc,init_fs_select ;не в диапазоне
	
	sub "A"
	ld (cur_drive),a ;сменить текущий диск
	
	;ld a,(cur_drive)
	sub 4 
	ld c,a
	ld b,0
	ld hl,typeDrive
	add hl,bc
	;инициализация
	ld a,(hl) ;выбранный раздел
	call DrvFAT.InitFAT32
	call c,print_hdd_error
	
	;в корневой каталог
	call DrvFAT.setRootDir
	call c,print_hdd_error

	;подсчет количества легинтимных (первых 256) записей в текущем каталоге	
	;call DrvFAT.GetNumOfEntries
	ret
	
	
	
readcat_fat
	call cat_cls ;почистить место
	xor a
	ld (files_l),a
	ld hl,cat_l
	ld (cat_target_tmp),hl ;указатель на начало
	;прочитать сектор каталога
	ld bc,0 ;первый сектор
	ld e,cat_size_max/512 ;максимум секторов влезет в буфер
readcat_fat_cl
	push de
	call DrvFAT.ReadSectorDIR
	pop de
	call c,print_hdd_error
	jr c,init_fs_ex
	
	
	push hl
	pop ix ;уазатель на запись о файле
	;перенести 1 сектор в каталог, кроме лишних записей
	push bc
	ld b,16 ;цикл записей в секторе
readcat_fat_cl_one
	push bc
	ld a,(ix)
	or a ;конец каталога
	jr z,readcat_fat_cl_skip
	cp #e5 ;удалённый
	jr z,readcat_fat_cl_skip
	cp #05 ;удалённый	
	jr z,readcat_fat_cl_skip	
	ld a,(ix+#0b)
	cp #0f ;длинный	
	jr z,readcat_fat_cl_skip
	cp #08 ;метка тома
	jr z,readcat_fat_cl_skip
	;проверка на папку "." (этот же каталог)
	ld a,(ix+#00)
	cp "." ;	
	jr nz,readcat_fat__add
	ld a,(ix+#01)
	cp " " ;
	jr z,readcat_fat_cl_skip
readcat_fat__add	
	;перенести одну запись
	push ix
	pop hl
	ld de,(cat_target_tmp)
	ld bc,32
	ldir
	ld (cat_target_tmp),de
	ld a,(files_l)
	inc a
	ld (files_l),a
	
readcat_fat_cl_skip	
	
	ld bc,32 ;на следующую запись
	add ix,bc
	pop bc
	djnz readcat_fat_cl_one	
	pop bc
	;

	
	
	ld hl,16 ;на следующий сектор (512/32)
	add hl,bc
	ld b,h
	ld c,l
	dec e
	jr nz,readcat_fat_cl

	
init_fs_ex
	; ld a,(cur_drive)
	; or a
	ret
cat_target_tmp dw 0 ;временно	
numDrives_last db 0 ;временно

wait_key ;ждёт любой клавиши
	halt
	ld a,(23556)
	cp 255
	jr z,wait_key
	ret
	
	
cat_cls ;очистить каталог слева
	ld hl,cat_l
	ld de,cat_l+1
	ld bc,cat_l_end-cat_l-1
	ld (hl),0
	ldir
	ret


;печать каталога левая панель FAT
printcat_l_fat
			ld a,(panel) ;проверка надо ли скрыть курсор
			cp "L"
			ld a,colorcatc_act	
			jr z,printcat_l_fat1
			ld a,colorcat_			
printcat_l_fat1
			ld (colorcatc_l),a
			
            ld      hl,col_pos_l ;
            xor		a
clscat_fat_l
            ld      d,h
            ld      e,l
            inc     de
            ld      c,a  ;keep
            ld      a,(shiftcat_l)
            ld      b,a
            ld      a,(curfile_l)
            and     a
            sbc     a,b
            cp      c
            ld      (cursor_l),a ;posit
            ld      a,(colorcat_l)
            jr      nz,clscat_fat01_l
            ld      a,(colorcatc_l) ;mark
clscat_fat01_l
            ld      (hl),a  ;раскраска атрибутами
            ld      a,c  ;restor
            ld      bc,9
            ldir
            ld      bc,32-9
            add     hl,bc
            inc     a
            cp      files_view
            jr      c,clscat_fat_l

       ;     ld      hl,catposit
       ;     call    print
	   
			ld a,(files_l)
			or a
			ret z ;выход если нет файлов
			
            ld      bc,cat_l  ;теперь печать имён файлов
            ld      a,(shiftcat_l)
			ld xl,a
            ; or      a
            ; jr      z,printcat00
			ld      l,a
			ld 		h,0
;printcat01  ;find name file
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add    hl,hl ;32
			add hl,bc
            ;dec    a
            ;jr    nz,printcat01
;printcat00

            xor     a
printcat_fat02_l
            push    af
            push    hl
                       ;print 24 row

            ld      (catposit_l+1),a
            ld      hl,catposit_l ;установим позицию печати
            call    print
            pop     hl
            push    hl
			call 	format_name_fat_l ;подготовим
			;отметка галкой
			ex de,hl
			ld hl,cat_mark_l
			ld a,xl
			add l
			ld l,a
			ld a,(hl)
			ex de,hl
			or a
			jr z,printcat_mark_fat_l
			push hl
			ld a,0-5 ;знак галка
			ld bc,8
			add hl,bc
			ld (hl),a
			pop hl
printcat_mark_fat_l			
			inc xl
			;
			
			ld bc,12 ;длина
            call    print_ ;печать одного имени файла
            ;ld      hl,catspace
            ;call    print
            pop     hl
            ld      bc,lenghtName*2 ;на след. имя
            add     hl,bc
            pop     af
            inc     a
			ld 		bc,(files_l) ;проверка сколько всего файлов
			cp 		c
			ret		nc
            cp      files_view ;проверка сколько файлов показывать
            jr      c,printcat_fat02_l
			ret	

;открыть папку слева
open_dir_l
			ld a,(curfile) ;текущ файл
			ld bc,cat_l
			ld l,a
			ld h,0
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add    hl,hl ;32
			add hl,bc
			push hl
			pop ix ;запомним
	ld a,(ix+#0b)
	bit 4,a ;это папка?
	jp z,wait
	ld e,0	
	;проверка на папку ".." (каталог выше)
	ld a,(ix+#00)
	cp "." ;	
	jr nz,open_dir_l1
	ld a,(ix+#01)
	cp "." ;	
	jr nz,open_dir_l1
	ld a,(ix+#02)
	cp " " ;
	jr nz,open_dir_l1
	ld e,#80 ;7,e =1 это возврат в родительский каталог
open_dir_l1
	ld a,(ix+#0b)
	call DrvFAT.Enter2DIR
	call c,print_hdd_error
	call    renewcat_l
    jp      wait

;повторный выбор раздела	
select_hdd
	call cls_l
	call init_fs_fat_prn2
	; ld a,24 ;скрыть курсор
	; ld (curfile),a
	; xor a
	; ld (shiftcat_),a
	call renewcat_l
	jp wait





copyF_LR_FAT	;копирование файлов слева направо FAT	
			ld a,(open_trd_flag)
			or a
			jp nz,wait ;если открыт образ справа, то нельзя


			;окно запроса с выбором диска
			ld a,(cur_drive)
			add a,"A"
			ld (mes_trd_dsk+3),a
			ld hl,mes_copy_file
			call print
			ld hl,mes_trd_dsk
			call print	
			ld a,col_win
			call paint_win
copyF_LR_FAT_w ;ожидание согласия
			halt
			ld 		a,(23556) ;сист. переменная нажатая клавиша 
			cp 		"N"
			jp z,copyF_exit ;выход если нет согласия
			cp 		"Y"
			jr nz,copyF_LR_FAT_w
			
			;узнать сколько отмеченных файлов
			ld hl,cat_mark_l
			ld b,128 ;размер таблицы
			ld xl,0 ;счётчик
copyF_LR_FAT_no_one_chk
			ld a,(hl)
			or a
			jr z,copyF_LR_FAT_no_one_chk1
			inc xl
copyF_LR_FAT_no_one_chk1
			inc hl
			djnz copyF_LR_FAT_no_one_chk
			inc xl
			dec xl
			jr nz,copyF_LR_FAT_no_one ;не один
			ld a,(curfile) ;тогда только текущий файл			
			call copyF_LR_FAT_one		
			jp nc,copyF_LR_fat_exit ;выход без ошибки
			cp 255
			jp z,copyF_LR_fat_exit ;выход после останова
			jp copyF_LR_fat_error ;выход с ошибкой
			
			
copyF_LR_FAT_no_one ;копировать несколько			
			ld hl,cat_mark_l
			ld a,xl
			ld (cat_mark_cur_tot),a
			xor a
copyF_LR_FAT_no_one_cl
			ld (cat_mark_cur_cl),a
			ld (cat_mark_cur),hl
			ld a,(hl) ;есть отметка?
			or a
			jr z,copyF_LR_FAT_no_one_cl1
			ld a,(cat_mark_cur_tot)
			ld l,a
			ld h,0
			dec a
			ld (cat_mark_cur_tot),a
			call print_progress_tot ;прогресс
			ld a,(cat_mark_cur_cl) ;очередной номер файла			
			call copyF_LR_FAT_one ;скопировать	
			jp nc,copyF_LR_FAT_no_one_cl1 ;продолжить без ошибки
			cp 255
			jp z,copyF_LR_fat_exit ;выход после останова
			jp copyF_LR_fat_error ;выход с ошибкой
			
copyF_LR_FAT_no_one_cl1			
			ld hl,(cat_mark_cur)
			ld (hl),0 ;снять отметку
			inc hl
			ld a,(cat_mark_cur_cl)
			inc a
			cp 128
			jr c,copyF_LR_FAT_no_one_cl
			
			jp copyF_LR_fat_exit ;без ошибки

copyF_LR_fat_exit
	call renewcat_r
	jp copyF_exit
	
copyF_LR_fat_error
	call renewcat_r
	jp copyF_error		
			
copyF_LR_FAT_one ;скопировать один файл слева направо
			;на входе в A - номер файла
			;на выходе C=1, если ошибка; A=255, если остановлено
		
			;узнать параметры файла
			ld bc,cat_l ;каталог диска слева
			;ld a,(curfile) ;текущ файл
			ld l,a
			ld h,0
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add    hl,hl ;32
			add hl,bc	

			ld (copyF_RL_source_file),hl ;указатель на запись о файле			
			ld bc,#1c+3
			add hl,bc
			ld a,(hl) ;размер файла 4й байт
			cp 4
			jp nc,copyF_LR_error ;если слишком большой
			ld c,a ;4й
			dec hl
			ld a,(hl) ;размер файла 3й байт
			ld d,a ;3й
			dec hl			
			ld a,(hl) ;
			ld e,a ;2й
			
			
			srl c ;/2 на старший бит придёт 0
			rr d ; 
			rr e ; на старший бит придёт флаг С
			srl c ;/4
			rr d ;
			rr e

			ld a,(hl) ;2й байт
			and %00000011
			ld c,a
			dec hl
			ld a,(hl) ;1й байт
			or c
			jr z,copyF_LR_FAT_one_fix
			inc de ;фикс
copyF_LR_FAT_one_fix	
	
			;ld (copyF_RL_source_sec_size),de ;два байта длины в секторах по 1024
			
			;цикл по размеру в секторах
			ld ix,de

		
			;начинаем копировать файл
		
			
			ld hl,(copyF_RL_source_file) ;указатель
			call format_name_fat_dot
			ld de,trn_buf+16 ;перекинем имя файла в запрос
			ld bc,file_name_len
			ldir
			ld a,4 ;Тип пакета Запрос на передачу файла 1024 байт
			ld (trn_buf+3),a
			
			ld hl,0
			ld (trn_buf+4),hl ;с нулевой части
			
			;размер файла
			ld hl,(copyF_RL_source_file) ;указатель
			ld bc,#1c
			add hl,bc
			ld a,(hl)
			ld (trn_buf+12),a 
			inc hl
			ld a,(hl)			
			ld (trn_buf+13),a 
			inc hl
			ld a,(hl)	
			ld (trn_buf+14),a 
			inc hl
			ld a,(hl)	
			ld (trn_buf+15),a
	
	
	
	;открыть файл
	ld hl,(copyF_RL_source_file) ;указатель
	push ix
	ld ix,file_fcb_tmp

;установка fcb буфера на основании дескриптора файла
;вх:  ix - адрес fcb буфера
;     hl - адрес дескриптора файла
;     (CLS_CurrDir) кластер каталога с файлом
;вых: hl - адрес дескриптора файла
	call DrvFAT.fcbSetFromEntry
	pop ix
	call c,print_hdd_error
	jp c,copyF_LR_error




			;цикл	
copyF_LR_FAT3			
			push ix
			pop hl
			call print_progress
		
		
	;считать 1 сектор 1024
	push ix
	ld ix,file_fcb_tmp
	ld hl,trn_buf+pack_size_32_com ;адрес отправляемого пакета
	ld bc,2*512
;чтение данных из файла в память
;  файл должен быть открыт
;  в fcb должны быть установлены: fcbClsFile, fcbOffset
;вх:  ix - адрес fcb
;     hl - адрес для чтения
;     bc - количество байт для чтения
	call DrvFAT.ReadDataFromFile
	pop ix
	call c,print_hdd_error
	jp c,copyF_LR_error

	call sec1024_to_srv ;отправить на сервер 
		jp c,copyF_LR_error
			
copyF_LR_FAT_check_pass	;проверка прошла

	
			ld hl,(trn_buf+4) ;следующая честь файла
			inc hl	
			ld (trn_buf+4),hl
			
			dec ix
			ld a,xl
			or xh
			jp nz,copyF_LR_FAT3

			
			;call cls_r ;очистить правую панель
			call printcat_l	
			;call renewcat_r	
			
			xor a ;С=0, нет ошибок
			ret


sec1024_to_srv
			;отправка сектора 1024 в образ. заголовок пакета уже должен быть готов
			call check_init_r ;авто проверка инициализации
			
			ld a,(23556) ;сист. переменная нажатая клавиша 
			cp " "
			jr nz,sec1024_to_srv2 ;продолжим
			ld hl,mes_stopped ;прервём
			call print_sys
			call wait_releas
			pop hl ;поправить адрес возврата
			ld a,255 ;ошибка останов
			ccf ;С=1
			ret
			
sec1024_to_srv2			
			
			
			ld de,trn_buf
			ld bc,pack_size_1024_com+pack_size_32_com
			ld (check_sum_size),bc
			call calc_check_sum ;проверить сумму
			ld (trn_buf+pack_size_1024_com+pack_size_32_com),hl ;записать её в пакет

			ld hl,trn_buf ;откуда
			ld de,pack_size_1024 ;размер
			call Wifi.tcpSend ;отправить
			jr c,sec1024_to_srv ;если ошибка, повтор
			
			ld hl,mes_trq_part_file ;сообщение пока ждём ответ
			call print_sys
			
			ld hl,rec_buf ;куда
			ld (Wifi.buffer_pointer),hl
			call Wifi.getPacket ;приём
			jr c,sec1024_to_srv ;если ошибка, повтор
			
			;принят пакет

			ld hl,mes_trn_part_file ;сообщение
			call print_sys
			ld hl,rec_buf
			ld bc,pack_size_32_com
			ld (check_sum_size),bc ;задать длину пакета для подсчёта
			ld c,5 ; Тип пакета Файл 1024
			call check_pack
			jr c,sec1024_to_srv ;если ошибка]повтор
			xor a
			ret ;выход без ошибок









copyF_RL_FAT ;копирование файлов справа налево FAT
			ld a,(open_trd_flag)
			or a
			jp nz,wait ;если открыт образ справа


			;узнать сколько отмеченных файлов
			ld hl,cat_mark_r
			ld b,128 ;размер таблицы
			ld xl,0 ;счётчик
copyF_RL_FAT_no_one_chk
			ld a,(hl)
			or a
			jr z,copyF_RL_FAT_no_one_chk1
			inc xl
copyF_RL_FAT_no_one_chk1
			inc hl
			djnz copyF_RL_FAT_no_one_chk
			inc xl
			dec xl
			jr nz,copyF_RL_FAT_no_one2 ;не ноль
			;не отмечено
copyF_RL_FAT_no_one2
			;окно запроса с выбором диска
			ld a,(cur_drive)
			add a,"A"
			ld (mes_trd_dsk+3),a
			ld hl,mes_copy_file
			call print
			ld hl,mes_trd_dsk
			call print	
			ld a,col_win
			call paint_win

copyF_RL_FAT_w ;ожидание согласия
			halt
			ld 		a,(23556) ;сист. переменная нажатая клавиша 
			cp 		"N"
			jp z,copyF_exit ;выход если нет согласия
			cp 		"Y"
			jr nz,copyF_RL_FAT_w
	
			inc xl
			dec xl
			jr nz,copyF_RL_FAT_no_one ;не один
			ld a,(curfile) ;тогда только текущий файл			
			call copyF_RL_FAT_one		
			jp nc,copyF_RL_fat_exit ;выход без ошибки
			cp 255
			jp z,copyF_RL_fat_exit ;выход после останова
			jp copyF_RL_fat_error ;выход с ошибкой	
			
			
copyF_RL_FAT_no_one ;копировать несколько			
			ld hl,cat_mark_r
			ld a,xl
			ld (cat_mark_cur_tot),a
			xor a
copyF_RL_FAT_no_one_cl
			ld (cat_mark_cur_cl),a
			ld (cat_mark_cur),hl
			ld a,(hl) ;есть отметка?
			or a
			jr z,copyF_RL_FAT_no_one_cl1
			ld a,(cat_mark_cur_tot)
			ld l,a
			ld h,0
			dec a
			ld (cat_mark_cur_tot),a
			call print_progress_tot ;прогресс
			ld a,(cat_mark_cur_cl) ;очередной номер файла			
			call copyF_RL_FAT_one ;скопировать	
			jp nc,copyF_RL_FAT_no_one_cl1 ;продолжить без ошибки
			cp 255
			jp z,copyF_RL_fat_exit ;выход после останова
			jp copyF_RL_fat_error ;выход с ошибкой			
copyF_RL_FAT_no_one_cl1			
			ld hl,(cat_mark_cur)
			ld (hl),0 ;снять отметку
			inc hl
			ld a,(cat_mark_cur_cl)
			inc a
			cp 128
			jr c,copyF_RL_FAT_no_one_cl
			
			jp copyF_RL_fat_exit ;выход без ошибки

copyF_RL_fat_exit
			call renewcat_l
			jp copyF_exit

copyF_RL_fat_error
			call renewcat_l
			jp copyF_error

copyF_RL_FAT_one ;по одному файлу справа налево FAT
			;на входе в A - номер файла
			;на выходе C=1, если ошибка; A=255, если остановлено
		
			;узнать параметры файла
			ld bc,cat_r ;адрес обычного каталога	
			;ld a,(curfile) ;текущ файл
			ld l,a
			ld h,0
            add    hl,hl ;2
			add    hl,hl ;4
			add    hl,hl ;8
			add    hl,hl ;16
			add hl,bc	

			ld (copyF_RL_source_file),hl ;указатель на запись о файле			
			
			
			ld bc,12+3
			add hl,bc
			ld a,(hl) ;размер файла 4й байт
			cp 4
			jp nc,copyF_LR_error ;если слишком большой
			ld c,a ;4й
			dec hl
			ld a,(hl) ;размер файла 3й байт
			ld d,a ;3й
			dec hl			
			ld a,(hl) ;
			ld e,a ;2й
			
			;разделить на 1024
			srl c ;/2 на старший бит придёт 0
			rr d ; 
			rr e ; на старший бит придёт флаг С
			srl c ;/4
			rr d ;
			rr e

			;цикл по размеру в секторах 1024
			ld ix,de

			;узнать размер последней части файла. обратно умножим на 1024
			;ld c,0
			sla e ;*2 на младший бит придёт 0
			;rl d ;на младший бит придёт флаг C
			;rl c
			sla e ;*2 на младший бит придёт 0
			;rl d ;на младший бит придёт флаг C
			;rl c
			;ld e,d
			;ld d,c ;в DE 3й и 4й байт
			ld d,e
			ld e,0
			ld hl,(copyF_RL_source_file) ;указатель на запись о файле			
			ld bc,12+1
			add hl,bc
			ld a,(hl) ;размер файла 2й байт	
			dec hl
			ld l,(hl) ;1й байт
			ld h,a
			and a
			sbc hl,de ;узнали остаток
			ld (copyF_FAT_RL_last_part),hl

			;коррекция числа пакетов
			ld hl,(copyF_RL_source_file) ;указатель на запись о файле			
			ld bc,12+1
			add hl,bc
			ld a,(hl) ;2й байт
			and %00000011
			ld c,a
			dec hl
			ld a,(hl) ;1й байт
			or c
			jr z,copyF_RL_FAT_one_fix
			inc ix ;фикс
copyF_RL_FAT_one_fix	
	
			
			;пробуем открыть файл
			push ix
			ld hl,(copyF_RL_source_file) ;указатель на запись о файле
			ld de,file_name_cur
			ld bc,file_name_len ;перенести, чтобы в конце был 0
			ldir
			xor a
			ld (de),a
			ld hl,file_name_cur
			call  DrvFAT.strTestFileName
			call c,print_hdd_error
			jp c,copyF_RL_FAT_one1      ;ошибка в имени
			ld  de,file_fcb_tmp
			call  DrvFAT.FileNameTo8dot3
			; ld  de,file_fcb_tmp
			; ld  bc,file_name_len
			; ldir
			ld  ix,file_fcb_tmp
			call  DrvFAT.fatEraseEntry
			call c,print_hdd_error
			jp c,copyF_RL_FAT_one1	
			; ld  hl,(CLS_CurrDir+0)
			; ld  de,(CLS_CurrDir+2)
			call  DrvFAT.fcbSetCLSCurrDir
			res  4,(ix+DrvFAT.fcbType)    ;файл
			ld  de,#0000
			ld  hl,#0000    ;size
			call  DrvFAT.DEHL2fcbSize
			call  DrvFAT.fatCreateFile
copyF_RL_FAT_one1	
			ld  hl,#FFFF
			ld  (SecDIRinBuf+2),hl
			ld  a,(PartitionNum)
			ld  (ix+DrvFAT.fcbPart),a
			;загрузка в fcbOffset указателя в файле
			ld de,0
			ld hl,0
			call DrvFAT.DEHL2fcbOffset
			pop ix
			call c,print_hdd_error
			jp c,copyF_LR_error	
	
	
			
			;начинаем копировать файл
		
			ld hl,(copyF_RL_source_file) ;указатель на запись о файле
	
			ld de,trn_buf+16 ;перекинем имя файла в запрос
			ld bc,file_name_len
			ldir
			ld a,2 ;Тип пакета Запрос приёма файла 1024 байт
			ld (trn_buf+3),a

			ld hl,0
			ld (trn_buf+4),hl ;с нулевой части
			
			;размер файла
			ld hl,(copyF_RL_source_file) ;указатель
			ld bc,12
			add hl,bc
			ld a,(hl)
			ld (trn_buf+12),a 
			inc hl
			ld a,(hl)			
			ld (trn_buf+13),a 
			inc hl
			ld a,(hl)	
			ld (trn_buf+14),a 
			inc hl
			ld a,(hl)	
			ld (trn_buf+15),a
			
			;цикл

copyF_RL_FAT3			
	call check_init_r ;авто проверка инициализации

			push ix
			pop hl
			call print_progress
			
			ld a,(23556) ;сист. переменная нажатая клавиша 
			cp " "
			jr nz,copyF_RL_FAT2 ;продолжим
			ld hl,mes_stopped ;прервём
			call print_sys
			call wait_releas
			ld a,255
			ccf ;C=1
			ret
			
copyF_RL_FAT2			
			ld hl,trn_buf ;откуда
			ld de,pack_size_32_com ;размер
			call Wifi.tcpSend ;отправить
			jr c,copyF_RL_FAT3 ;если ошибка, то новая попытка
			
			ld hl,mes_req_part_file ;сообщение пока ждём ответ
			call print_sys
			
			ld hl,rec_buf ;куда
			ld (Wifi.buffer_pointer),hl
			call Wifi.getPacket ;приём
			jr nc,copyF_RL_FAT_check_pass1
			jr copyF_RL_FAT3 ;если ошибка, то новая попытка
			
copyF_RL_FAT_check_pass1	;принят пакет
			ld hl,mes_rec_part_file ;сообщение
			call print_sys
	ld hl,rec_buf			
	ld bc,pack_size_1024_com+pack_size_32_com
	ld (check_sum_size),bc ;задать длину пакета для подсчёта
			ld c,3 ; Тип пакета Файл 1024
			call check_pack
			jr nc,copyF_RL_FAT_check_pass
			;call init_dev
			jp copyF_RL_FAT3 ;если ошибка, то новая попытка
			
copyF_RL_FAT_check_pass	;проверка прошла



	;запишем 1 сектор 1024
	push ix
	ld bc,2*512 ;на запись 1024
	;проверка на последнюю часть файла
	ld a,xh
	or a
	jr nz,copyF_RL_FAT_write_no_last
	ld a,xl
	cp 1
	jr nz,copyF_RL_FAT_write_no_last
	ld hl,(copyF_FAT_RL_last_part)	;остаток
	ld a,h
	or l
	jr z,copyF_RL_FAT_write_no_last ;если = 0
	ld b,h
	ld c,l
	
copyF_RL_FAT_write_no_last	
	ld ix,file_fcb_tmp
	ld hl,rec_buf+32 ;адрес принятого пакета

;запись данных из памяти в файл
;  файл должен быть открыт
;  в fcb должны быть установлены: fcbName, fcbExt, fcbClsFile, fcbClsDIR,
;    fcbSize, fcbOffset, fcbPart
;вх:  ix - адрес fcb
;     hl - адрес в памяти для записи
;     bc - количество байт для записи
;вых: cy=1 ошибки
;        a=errNumTooBig - слишком большой размер, более #FFFF кластеров
;        a=errRWnum - код ошибки
;        a=errFileEmpty - нулевой размер файла
;        a=errDiskNoSpace
;        a=errInvalidPart
;        a=errEoF - файл прервался
;        a=errFileNotFound
;     cy=0 данные записаны
;       hl следующий адрес для записи
	call DrvFAT.WriteDataToFile
	pop ix
	call c,print_hdd_error
	jp c,copyF_LR_error
	

			ld hl,(trn_buf+4) ;следующая честь файла
			inc hl	
			ld (trn_buf+4),hl

			dec ix
			ld a,xl
			or xh
			jp nz,copyF_RL_FAT3

			
			call printcat_l	
			call printcat_r
			xor a ;C=0
			ret
	

	
	
	
	
	
	
;узнать дексриптор файла фат из каталога слева
;вх: A - номер файла в каталоге
;вых: ix - дескриптор
calc_cat_fat_deskr
			ld      ixl,a
			ld 		ixh,0
            add    ix,ix ;2
			add    ix,ix ;4
			add    ix,ix ;8
			add    ix,ix ;16
			add    ix,ix ;32
			ld bc,cat_l
			add ix,bc
			ret



format_name_fat_dot ;подогнать имя 8+3 под стандарт filename.ext
;вх: HL - имя в каталоге
	push hl
	ld hl,file_name_cur
	ld de,file_name_cur+1 ;почистить
	ld bc,file_name_len-1
	ld (hl),0
	ldir
	
	pop hl
	push hl
	
	ld de,file_name_cur ;куда 
	ld bc,#08ff 
format_name_fat_dot_cl
	ld a,(hl)
	cp " "+1
	jr c,format_name_fat_dot_skip
	ldi
	djnz format_name_fat_dot_cl
format_name_fat_dot_skip
	ld a,"."
	ld (de),a
	
	inc de
	pop hl
	ld bc,8 ;перейти на расширение
	add hl,bc
	
	ld bc,#03ff
format_name_fat_dot_cl2
	ld a,(hl)
	cp " "+1
	jr c,format_name_fat_dot_skip2
	ldi
	djnz format_name_fat_dot_cl2
format_name_fat_dot_skip2
	xor a ;в конце 0
	ld (de),a
	ld hl,file_name_cur
	ret

;печать номера ошибки HDD в системной строке
print_hdd_error
	push af
	;пропуск некоторых ошибок
	cp #71
	jr z,print_hdd_error_skip
	push hl
	push de
	push bc
	push ix
	ld c,a
	and %11110000
	srl a
	srl a
	srl a
	srl a
	call print_hdd_error_to_hex
	ld (mes_hdd_error_num),a
	ld a,c
	and %00001111
	call print_hdd_error_to_hex
	ld (mes_hdd_error_num+1),a
	ld hl,mes_hdd_error
	call print_sys
	pop ix
	pop bc
	pop de
	pop hl
print_hdd_error_skip
	pop af
	ret
print_hdd_error_to_hex
	cp 10
	jr nc,print_hdd_error_to_hex1
	add "0" ;0-9
	ret
print_hdd_error_to_hex1 ;A-F
	sub 10
	add "A"
	ret
	
mes_hdd_error db "Disk error #"
mes_hdd_error_num db "00",0
	
		;align 256
font    insert  "FONT.C" ;шрифт

	;org #a000
	;incbin "_cat.txt" ;тестовый каталог
	
;для драйвера HDD
PartitionNum db 0 ;
_ExtFlags db 0 ;флаги внешней программы
_BufSecHdd ds #200 ;буфер для драйвера HDD
_VarsFAT ds #5B ;адрес блока переменных
;различные буфера
_Buf4File	ds #200 ;буфер для чтения файла
_Buf4MBR	equ _BufSecHdd ;ds #200 ;буфер с сектором MBR
_Buf4LFN	ds #100 ;буфер для создания записей с длинным именем
_Buf4DIR	ds #200 ;буфер для загрузки сектора DIR (каталога)
_Buf4FAT	ds #200 ;буфер для загрузки сектора FAT
_CurrentPath ds #100 ;буфер текущего пути
_tmp_fcb	ds #20 ;для временного хранения fcb
BufIDsd ds #200 ;буфер для SD (нужен?)

_msSMUC	  ds	11		;SMUC master
_slSMUC	  ds	11		;SMUC slave
_msNemo	  ds	11		;Nemo master
_slNemo	  ds	11		;Nemo slave
_msATM	  ds	11		;ATM IDE master
_slATM	  ds	11		;ATM IDE slave
_msPROFI	  ds	11		;Profi IDE master
_slPROFI	  ds	11		;Profi IDE slave
_cardZC	  ds	11		;ZC SD Є ав 
_mDivMMC	  ds	11		;DivMMC master
_sDivMMC	  ds	11		;DivMMC slave

		include "Drivers/InitHDD/init.a80"
		include "Drivers/InitHDD/init2.a80"		
		include "Drivers/DrvHDD/DrvHDD.main.a80"
		include "Drivers/DrvFAT/DrvFAT.main.a80"
		
		align 8
trn_buf	db	"ZXN" ;тут пакет для передачи 

end_ ;ниже буферы, в файл не выгружаются

	ds 2048
	
cipstart_line ds 256 ;буфер для строки соединения
	
file_fcb_tmp ds 32 ;временно
file_name_cur ds file_name_len+3 ;временно
file_cfg_name_buf ds 256


	align 8
cat_r2 ds  2048 ;буфер каталога справа (временная копия)
	align 8
cat_open_trd ds 9*256+16 ;буфер каталога открытого образа (правая панель)
	align 128
cat_mark_l ds 128 ;база отмеченных файлов слева
cat_mark_r ds 128 ;база отмеченных файлов справа
cat_l ds cat_size_max ;9*256 ;буфер каталога диска (левая панель)
cat_l_end
cat_r ds 2048 ;буфер каталога справа
cat_r_end
rec_buf ds  2048 ;буфер приёма

;сохранение файла настроек
start_ini
	incbin 'AY232K_i.C'
end_ini

	SAVETRD "AY232K.TRD",|"AY232K_i.C",start_ini,end_ini-start_ini
;сохранение
end_all
	SAVETRD "AY232K.TRD",|"AY232K.C",start_,end_-start_
