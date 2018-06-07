%%%%----------------------------------------------
%%% Module: Account number management commands handler 
%%% Description: PDF form filler
%%% Author:
%%% Autogen: Yi qi
%%% wiki page -> [link to wiki or page name]
%%%%----------------------------------------------
-module(pdf).
-compile([export_all]).

%Для работы с pdf нужно сделать отдельный модуль.
%pdf:make(Template, Fields, Options)

%% проблемы с utf8 
%% https://bugs.launchpad.net/ubuntu/+source/pdftk/+bug/663603




%make(Template, Fields, Options) ->
make(Template, Options) ->
%++ integer_to_list(rand:uniform(9999))
	
	Fields = [
		{payment_ordno,"untitled8","Номер","В-1234" },
		{payment_date_day_month,"untitled9","ДeньМесяц","06.05"},
		{payment_date_year,"untitled10","Год","18"},
%		{payment_amount,"","Сумма",""},
%		{payment_sattlement,"","ВидОплаты",""},
%		{payment_time_constraints,"","СрокПлатежа",""},
%		{payment_priority,"","Очередность",""},
%		{payment_type,"","ВидПлатежа",""},
%		{payment_withdraw_date,"","ДатаСписано",""},
%		{payment_purpose,"","НазначениеПлатежа",""},
		{payer_name,"untitled11","Плательщик","-"},
		{payee_director,"untitled13","ПлательщикРуководитель","Штурман-М.А."},
		{payee_accountant,"untitled14","ПлательщикБухгалтер","Счетоводова-А.Г."},
		{payee_name2,"untitled12","Получатель","ООО-\"Грузовички\""},
		{payee_name1,"untitled4","Получатель","ООО-\"Грузовички\""},
		{payee_kpp,"untitled3","ПолучательКПП","12345678"},
		{payee_inn,"untitled2","ПолучательИНН","987654324"},
		{payee_account,"untitled7","ПолучательСчет","40857576347656576588"},
		{payee_bank,"untitled1","ПолучательБанк","ПАО-СБЕРБАНК-Г.МОСКВА"},
		{payee_bik,"untitled5","ПолучательБИК","9328475"},
		{payee_ks,"untitled6","ПолучательКорсчет","3098679586798546437478"}
	],

	[{_,_,_,OrdNo}|_] = Fields,
	lists:foreach(
		fun({K,Fname,Fdesc,Fvalue}) ->
			io:format("~p ~p:~ts  #~ts~n",[K,Fname, Fvalue, Fdesc])
		end, 
	Fields),
	
	RenderedFields = lists:foldl(
		fun({K,Fname,Fdesc,Fvalue}=F,R) ->
			case R of
				[] -> R ++ [Fname,":",Fvalue];
				_ -> R ++ [" ",Fname,":",Fvalue]
			end  
		end,
	[],Fields),
	io:format("~ts~n", [RenderedFields]),
	Res = os:cmd("../bin/make_pdf.sh " ++ OrdNo ++ " " ++ RenderedFields),
	io:format("DONE -> ~ts~n", [Res]).



chomp(S) -> re:replace(S, "[\\n]$", "", [{return, list}]).
