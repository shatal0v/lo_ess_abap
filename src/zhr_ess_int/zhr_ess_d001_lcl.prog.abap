*&---------------------------------------------------------------------*
*&  Include           ZHR_ESS_D001_LCL
*&---------------------------------------------------------------------*
CLASS lcl_ess_d001 DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING iv_folder TYPE string OPTIONAL
           , get_data
           , end
           .
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_header,
             organizationuid          TYPE string,
             limitnumbers             TYPE i,
             staffnumbers             TYPE i,
             currentnumbers           TYPE i,
             vacancynumbers           TYPE i,
             informationsystemname    TYPE string,
             informationsystemversion TYPE string,
             importtypeid             TYPE string,
             datecreation             TYPE text100,
           END OF ty_header,
           BEGIN OF ty_plans_count,
             plans TYPE hrobjid,
           END OF ty_plans_count,
           ty_t_excluded_objid TYPE HASHED TABLE OF hrp1000-objid WITH UNIQUE KEY primary_key COMPONENTS table_line.
    DATA: BEGIN OF ms_prop
        " параметры:
        , otype  TYPE otype
        , objid  TYPE RANGE OF hrobjid
        , pernr  TYPE RANGE OF pernr_d
        , begda  TYPE RANGE OF datum   "MAKAROVSV 16.09.2019
        , folder TYPE string

        , orgeh TYPE flag "Подразделения
        , stell TYPE flag "Должности
        , plans TYPE flag "Штатные единицы

        , per00 TYPE flag "Общие сведения
        , per01 TYPE flag "Воинский учет
        , per02 TYPE flag "Образование
        , per03 TYPE flag "Трудовая деятельность в государственном органе
        , per04 TYPE flag "Трудовая должность до замещения должности в государственном органе
        , per05 TYPE flag "Сведения об окладе и надбавках
        , per06 TYPE flag "Состав семьи
        , per07 TYPE flag "Повышение квалификации, Профессиональная переподготовка
        , per08 TYPE flag "Аттестация
        , per09 TYPE flag "Отпуск
        , per10 TYPE flag "Классный чин, воинское звание
        , per11 TYPE flag "Сведения об увольнении
        , per12 TYPE flag "Языки                                       "KLOKOVNYU27.05.2019
        , per13 TYPE flag "Послевузовское профессиональное образование "KLOKOVNYU27.05.2019
        , per14 TYPE flag "Награды (поощрения), почетные звания        "KLOKOVNYU27.05.2019
        , per15 TYPE flag "Социальные льготы                           "KLOKOVNYU27.05.2019
        , sub22 TYPE RANGE OF p0022-subty
        , itype TYPE string
        , plvar TYPE plvar "Вариант плана
        , END OF ms_prop
        "Подразделения
        , BEGIN OF ms_organization
        , organizationid TYPE string "Уникальный идентификатор  OrganizationID
        , code           TYPE string "Код подразделения
        , shortname      TYPE string "Короткое название подразделения
        , name           TYPE string "Название подразделения
        , parentid       TYPE string "Код вышестоящего подразделения
        , creationdate   TYPE string "Дата создания
*{ KLOKOVNYU 17.06.2019
        , namegenitive   TYPE string
        , namedative     TYPE string
        , nameaccusative TYPE string
        , hierarchyname  TYPE string
        , abolitiondate  TYPE string "Дата упразднения   "MAKAROVSV 16.09.2019
*} KLOKOVNYU 17.06.2019
        , END OF ms_organization
        "Должности
        , BEGIN OF ms_position
        , positionid      TYPE string "Уникальный идентификатор
        , code            TYPE string "Код должности
        , shortname	      TYPE string "Короткое название должности
        , name            TYPE string "Название должности
        , categoryname    TYPE string "Категория должности
        , groupname	      TYPE string "Группа должности
        , isstateposition	TYPE string "Признак государственной должности
        , postsalary      TYPE string "Значение оклада
        , datestart	      TYPE string "Дата начала действия
*{ KLOKOVNYU 17.06.2019
        , namegenitive   TYPE string
        , namedative     TYPE string
        , nameaccusative TYPE string
*} KLOKOVNYU 17.06.2019
        , END OF ms_position
        "Штатные единицы
        , BEGIN OF ms_staffunit
        , staffunitid	    TYPE string "Уникальный идентификатор
        , organizationid  TYPE string "Идентификатор подразделения
        , positionid      TYPE string "Идентификатор должности
        , datestart	      TYPE string "Дата создания
        , dateend  	      TYPE string "Дата упразднения
        , END OF ms_staffunit
        "Общие сведения
        , BEGIN OF ms_common
        , personid                   TYPE string "Идентификатор сотрудника
        , lastname                   TYPE string "Фамилия
        , firstname	                 TYPE string "Имя
        , middlename                 TYPE string "Отчество
        , dateofbirth	               TYPE string "Дата рождения
        , gender                     TYPE string "Пол
        , taxcertificatenumber       TYPE string "Идентификационный номер налогоплательщика
        , insurancecertificatenumber TYPE string "Номер страхового свидетельства государственного пенсионного страхования
        , experiencegeneralyear	     TYPE string "Общий стаж лет (до)
        , experiencegeneralmonths	   TYPE string "Общий стаж месяцев (до)
        , experiencegeneraldays	     TYPE string "Общий стаж дней (до)
        , experiencecontinuousyear   TYPE string "Стаж непрерывеый количество лет (до)
        , experiencecontinuousmonths TYPE string "Стаж непрерывеый количество месяцев (до)
        , experiencecontinuousdays   TYPE string "Стаж непрерывеый количество дней (до)
        , experiencestateyear	       TYPE string "Стаж государственной (муниципальной) службы лет (до)
        , experiencestatelmonths     TYPE string "Стаж государственной (муниципальной) службы месяцев (до)
        , experiencestatedays	       TYPE string "Стаж государственной (муниципальной) службы дней (до)
        , maritalstatusokin	         TYPE string "Состояние в браке
        , passportseries             TYPE string "Паспорт серия
        , passportnumber             TYPE string "Паспорт номер
        , passportissued             TYPE string "Паспорт дата выдачи
        , passportissuedby           TYPE string "Паспорт кем выдан
        , personaltabelnumber	       TYPE string "Табельный номер
        , numservicecontract         TYPE string "Номер трудового договора (контракт)
        , dateservicecontract	       TYPE string "Дата трудового договора (контракт)
*{ KLOKOVNYU 27.05.2019
        ,passportissuedcode          TYPE string
        ,addressofregistrationindex  TYPE string
        ,addressofregistration       TYPE string
        ,hasrelative                 TYPE string
        ,hascertification            TYPE string
        ,hasadditionaleducation      TYPE string
        ,hasaward                    TYPE string
        ,hassocialbenefit            TYPE string
        ,hasrank                     TYPE string
*} KLOKOVNYU 27.05.2019
*{ MAKAROVSV 16.09.2019
        , birthplace                   TYPE string "Место рождения
        , citizenshipokin              TYPE string "Гражданство
        , medicalinsurancepolicynumber TYPE string "Номер полиса обязательного медицинского страхования
        , addressofresidenceindex      TYPE string "Индекс фактического места жительства
        , addressofresidence           TYPE string "Адрес фактического места жительства
        , workphone                    TYPE string "Номер телефона
*} MAKAROVSV 16.09.2019
        , END OF ms_common
        "Воински учет
        , BEGIN OF ms_military
        , personid                  TYPE string "Идентификатор сотрудника
        , militarystockcategory	    TYPE string "Категория запаса
        , militaryrank              TYPE string "Воинское звание
        , militarycomposition	      TYPE string "Состав (профиль)
        , militaryvuskode	          TYPE string "Полное кодовое обозначение ВУС
        , militaryreadycategory	    TYPE string "Категория годности к военной службе
        , militarycommissariat      TYPE string "Наименование военного комиссариата по месту жительства
        , END OF ms_military
        "Образование
        , BEGIN OF ms_education
        , educationbasicid          TYPE string "Идентийикатор
        , personid                  TYPE string "Идентификатор сотрудника
        , educationokin             TYPE string "Образование ОКИН
        , institutiontext	          TYPE string "Наименование образовательного учреждения
        , datestart	                TYPE string "Год начала
        , enddate	                  TYPE string "Год окончания
        , namedocumenteducation     TYPE string "Наименование документа об образовании
        , educationdocseries        TYPE string "Серия диплома
        , educationdocnumber        TYPE string "Номер диплома
        , qualification	            TYPE string "Квалификация по документуоб образовании ОКСО
        , speciality                TYPE string "Направление или специальность по документу (текст)
        , specialityokso            TYPE string "Направление или специальность по документу код по ОКСО
        , education	                TYPE string "Уровнь образования
        , flagmain                  TYPE string "Признак приоритетного образования
*{KLOKOVNYU 06.06.2019
        ,withhonorsdiploma TYPE string "Диплом с отличием
        ,educationdocdate  TYPE string
*{KLOKOVNYU 06.06.2019
        , END OF ms_education
*{ KLOKOVNYU 19.06.2019
        , BEGIN OF ms_education_grad
        , educationgraduateid       TYPE string "Идентийикатор
        , personid                  TYPE string "Идентификатор сотрудника
*        , educationokin             TYPE string "Образование ОКИН
        , institutiontext	          TYPE string "Наименование образовательного учреждения
        , education                 TYPE string "Уровень послевузовского образования
        , datestart	                TYPE string "Год начала
        , enddate	                  TYPE string "Год окончания
        , namedocumenteducation     TYPE string "Наименование документа об образовании
*        , educationdocseries        TYPE string "Серия диплома
        , educationdocnumber        TYPE string "Номер диплома
        , educationdocdate          TYPE string "Номер диплома
*        , qualification              TYPE string "Квалификация по документуоб образовании ОКСО
        , speciality                TYPE string "Направление или специальность по документу (текст)
        , specialityokso            TYPE string "Направление или специальность по документу код по ОКСО
*        , education                  TYPE string "Уровнь образования
*        , flagmain                  TYPE string "Признак приоритетного образования
*{KLOKOVNYU 06.06.2019
        , academicdegree            TYPE string "звание академика
*        ,withhonorsdiploma TYPE string "Диплом с отличием
*        ,educationdocdate  TYPE string
*{KLOKOVNYU 06.06.2019
        , END OF ms_education_grad
*} KLOKOVNYU 19.06.2019
       "Трудовая деятельность в государственном органе
        , BEGIN OF ms_orderappointment
        , orderappointmentid 	      TYPE string "Уникальный идентификатор
        , personid                  TYPE string "Идентификатор сотрудника
        , organizationid            TYPE string "Идентификатор подразделения
        , positionid                TYPE string "Идентификатор должности
        , ordernumb	                TYPE string "Номер приказа
        , orderdate	                TYPE string "Дата приказа
        , datestart	                TYPE string "Дата назначения
        , dateend	                  TYPE string "Дата окончания
        , sherate	                  TYPE string "Ставка
        , orderperiodtype	          TYPE string "Характер работы
        , flagmain                  TYPE string "Вид работы
        , staffunitid               TYPE string "Номер штатной должности "Klokovnyu 13.06.2019
        , substitutepersonid        TYPE string "Идентификатор замещенного сотрудника "MAKAROVSV 20.09.2019
        , END OF ms_orderappointment

*       Трудовая должность до замещения должности в государственном органе
        , BEGIN OF ms_workactivitie
        , workactivitieid           TYPE string "Уникальный идентификатор
        , personid                  TYPE string "Идентификатор сотрудника
        , organizationtext          TYPE string "Наименование организации и подразделения
        , addressorganization       TYPE string "Адрес организации
        , positiontext              TYPE string "Наименование должности
        , datestart                 TYPE string "Дата начала
        , dateend                   TYPE string "Дата окончания
        , experiencegeneral         TYPE string "Относится к общему стажу (признак)
        , experiencestate           TYPE string "Относится к стажу государственной службы (признак)
        , END OF ms_workactivitie

*       Сведения об окладе и надбавках
        , BEGIN OF ms_ordersalary
        , ordersalaryid             TYPE string "Идентификатор
        , personid                  TYPE string "Идентификатор сотрудника
        , orderappointmentid        TYPE string
        , positionsalaryvalue       TYPE string "Значение оклада
        , orderrankid               TYPE string
        , ordernumb                 TYPE string "Номер приказа
        , orderdate                 TYPE string "Дата приказа
        , datestart                 TYPE string "Дата начала действия
        , dateend                   TYPE string "Дата окончания
        , salarytypeid              TYPE string "Вид надбавки
        , salaryvalue               TYPE string "Величина
        , salaryvalueruble          TYPE string "Величина в рублях
        , END OF ms_ordersalary

*       Состав семьи
        , BEGIN OF ms_relatives
        , relativeid  TYPE string
        , personid    TYPE string
        , kinshipokin TYPE string
        , lastname    TYPE string
        , firstname   TYPE string
        , middlename  TYPE string
        , dateofbirth TYPE string
        , END OF ms_relatives

        , BEGIN OF ms_educationadditional
        , educationadditionalid TYPE string
        , personid              TYPE string
        , startdate             TYPE string
        , educationstype        TYPE string
        , dpoid                 TYPE string
        , institutiontext       TYPE string
        , speciality            TYPE string
        , namedocumenteducation TYPE string
        , educationdocnumber    TYPE string
        , educationdocdate      TYPE string
        , enddate               TYPE string
        , reason                TYPE string
        , END OF ms_educationadditional

        , BEGIN OF ms_certifications
        , certificationid TYPE string
        , personid        TYPE string
        , date            TYPE string
        , resulttext      TYPE string
        , ordernumber     TYPE string
        , orderdate       TYPE string
        , reason          TYPE string
        , END OF ms_certifications

        , BEGIN OF ms_holidays
        , holidayid   TYPE string
        , personid    TYPE string
        , holidaytype TYPE string
        , ordernumb   TYPE string
        , orderdate   TYPE string
        , countdays   TYPE string
        , datestart   TYPE string
        , dateend     TYPE string
* {KLOKOVNYU 19.06.2019
        , perioddatestart TYPE string
        , perioddateend TYPE string
        , periodcountdays TYPE string
        , periodcountdaysremain TYPE string
*} KLOKOVNYU 19.06.2019
        , END OF ms_holidays

        , BEGIN OF ms_orderranks
        , orderrankid TYPE string
        , personid    TYPE string
        , datestart   TYPE string
        , ordernumb   TYPE string
        , orderdate   TYPE string
        , rankid      TYPE string
        , END OF ms_orderranks

        , BEGIN OF ms_orderdismissals
        , orderdismissalid  TYPE string
        , personid          TYPE string
        , ordernumb         TYPE string
        , orderdate         TYPE string
        , datestart         TYPE string
        , dismissalreason   TYPE string
        , END OF ms_orderdismissals
*{KLOKOVNYU 19.06.2019
        , BEGIN OF ms_orderaward
        , orderawardid  TYPE string
        , personid          TYPE string
        , awardkind         TYPE string
        , awardtype         TYPE string
        , awardname         TYPE string
        , namedocumentaward TYPE string
        , numberawarding    TYPE string
        , dateawarding      TYPE string
        , END OF ms_orderaward
*}KLOKOVNYU 19.06.2019
*{ MAKAROVSV 20.09.2019
        , BEGIN OF ms_foreignlanguage
        , foreignlanguageid  TYPE string
        , personid           TYPE string
        , languageokin       TYPE string
        , languagedegreeokin TYPE string
        , END OF ms_foreignlanguage
        , BEGIN OF ms_socialsecurity
        , socialsecurityid   TYPE string
        , personid           TYPE string
        , socialbenefit      TYPE string
        , docnumber          TYPE string
        , datestart          TYPE string
        , dateend            TYPE string
        , reason             TYPE string
        , END OF ms_socialsecurity
*} MAKAROVSV 20.09.2019
        , mt_ordersalary            LIKE TABLE OF ms_ordersalary
        , mt_relatives              LIKE TABLE OF ms_relatives
        , mt_workactivitie          LIKE TABLE OF ms_workactivitie
        , mt_orderappointment       LIKE TABLE OF ms_orderappointment
        , mt_education              LIKE TABLE OF ms_education
        , mt_military               LIKE TABLE OF ms_military
        , mt_common                 LIKE TABLE OF ms_common
        , mt_staffunit              LIKE TABLE OF ms_staffunit
        , mt_position               LIKE TABLE OF ms_position
        , mt_organization           LIKE TABLE OF ms_organization
        , mt_educationadditional    LIKE TABLE OF ms_educationadditional
        , mt_certifications         LIKE TABLE OF ms_certifications
        , mt_holidays               LIKE TABLE OF ms_holidays
        , mt_orderranks             LIKE TABLE OF ms_orderranks
        , mt_orderdismissals        LIKE TABLE OF ms_orderdismissals
*{19.06.2019 KLOKOVNYU
        , mt_education_grad         LIKE TABLE OF ms_education_grad
        , mt_orderaward             LIKE TABLE OF ms_orderaward
*}19.06.2019 KLOKOVNYU
*{ MAKAROVSV 20.09.2019
        , mt_foreignlanguage        LIKE TABLE OF ms_foreignlanguage
        , mt_socialsecurity         LIKE TABLE OF ms_socialsecurity
*} MAKAROVSV 20.09.2019
        , BEGIN OF ms_data
        , header                    TYPE ty_header
        , organization              LIKE mt_organization
        , position                  LIKE mt_position
        , staffunit                 LIKE mt_staffunit
        , common                    LIKE mt_common
        , military                  LIKE mt_military
        , education                 LIKE mt_education
        , orderappointment          LIKE mt_orderappointment
        , workactivitie             LIKE mt_workactivitie
        , ordersalary               LIKE mt_ordersalary
        , relatives                 LIKE mt_relatives
        , educationadditional       LIKE mt_educationadditional
        , certifications            LIKE mt_certifications
        , holidays                  LIKE mt_holidays
        , orderranks                LIKE mt_orderranks
        , orderdismissals           LIKE mt_orderdismissals
*{19.06.2019 KLOKOVNYU
        , education_grad            LIKE mt_education_grad
        , orderaward                LIKE mt_orderaward
*}19.06.2019 KLOKOVNYU
*{ MAKAROVSV 20.09.2019
        , foreignlanguage           LIKE mt_foreignlanguage
        , socialsecurity            LIKE mt_socialsecurity
*} MAKAROVSV 20.09.2019
        . INCLUDE STRUCTURE ms_prop AS prop
    .
    DATA: END OF ms_data

        , mo_get_data TYPE REF TO zcl_hr_get_data

        , mt_t502t    TYPE HASHED TABLE OF t502t WITH UNIQUE KEY famst
        ", mt_t518b    TYPE HASHED TABLE OF t518b WITH UNIQUE KEY ausbi

        , ms_p0001  TYPE p0001
        , ms_p0000  TYPE p0000
        , mv_cdate  TYPE datum "контрольная дата
        , mt_plans_count TYPE HASHED TABLE OF ty_plans_count WITH UNIQUE KEY plans
        , mt_excluded_objid TYPE ty_t_excluded_objid
        .
    METHODS: read_p1000_data IMPORTING iv_otype TYPE otype
                                       iv_objid TYPE hrobjid
                             EXPORTING ev_code  TYPE string
                                       ev_datum TYPE string
                                       ev_sname TYPE string
           , read_p1005_data IMPORTING iv_otype TYPE otype
                                       iv_objid TYPE hrobjid
                             EXPORTING ev_cpmin TYPE string
           , sen_calculate   IMPORTING iv_pernr TYPE pernr_d
                                       iv_begda TYPE begda
                                       iv_proce TYPE t525p-proce
                             RETURNING VALUE(rs_durat) TYPE psen_duration_dec
           , summ_experiens IMPORTING is_exp1 TYPE psen_duration_dec
                                      is_exp2 TYPE psen_duration_dec
                            RETURNING VALUE(rs_durat) TYPE psen_duration_dec
           , get_upper_orgeh
              IMPORTING i_objid TYPE hrobjid
              RETURNING VALUE(r_obj) TYPE hrobjid

           , read_order
                IMPORTING i_pernr TYPE persno
                          i_begda TYPE begda
                          i_massn TYPE massn OPTIONAL
                EXPORTING
                  es_p0298 TYPE p0298


           , read_p0001 IMPORTING iv_pernr TYPE persno
           , read_p0000 IMPORTING iv_pernr TYPE persno
           , fill_common                IMPORTING iv_struc TYPE struc
           , fill_military              IMPORTING iv_struc TYPE struc
           , fill_education             IMPORTING iv_struc TYPE struc
           , fill_orderappointment      IMPORTING iv_struc TYPE struc
           , fill_workactivitie         IMPORTING iv_struc TYPE struc
           , fill_ordersalary           IMPORTING iv_struc TYPE struc
           , fill_relatives             IMPORTING iv_struc TYPE struc
           , fill_educationadditionals  IMPORTING iv_struc TYPE struc
           , fill_certifications        IMPORTING iv_struc TYPE struc
           , fill_holidays              IMPORTING iv_struc TYPE struc
           , fill_orderranks            IMPORTING iv_struc TYPE struc
           , fill_orderdismissals       IMPORTING iv_struc TYPE struc
           , fill_header
           , fill_education_grad        IMPORTING iv_struc TYPE struc "KlokovNYU 19.06.2019
           , fill_orderaward            IMPORTING iv_struc TYPE struc "KlokovNYU 19.06.2019
           , fill_foreignlanguage       IMPORTING iv_struc TYPE struc "MAKAROVSV 20.09.2019
           , fill_socialsecurity        IMPORTING iv_struc TYPE struc "MAKAROVSV 20.09.2019
           , check_mandatory_fld CHANGING cv_fld TYPE any
           , format_pernr CHANGING cv_fld TYPE string
*{ MAKAROVSV 23.09.2019
           , cacheid
              IMPORTING
                i_personid         TYPE zthr_ess_d001-personid
                i_block            TYPE zthr_ess_d001-block
                i_block_type       TYPE zthr_ess_d001-block_type
                i_block_date       TYPE zthr_ess_d001-block_date
                i_last_date        TYPE zthr_ess_d001-last_date
              RETURNING VALUE(r_block_id)  TYPE zthr_ess_d001-block_id
           , get_substitutepersonid
              IMPORTING
                i_objid           TYPE hrobjid
                i_begda           TYPE begda
                i_endda           TYPE begda
              EXPORTING
                e_prozt           TYPE string
              RETURNING VALUE(r_sobid)    TYPE sobid
           , get_common_hascertification
               IMPORTING  iv_struc    TYPE struc
               RETURNING VALUE(r_res) TYPE char1
           , get_common_hasadditionaleducat
               IMPORTING  iv_struc    TYPE struc
               RETURNING VALUE(r_res) TYPE char1
           , get_common_hassocialbenefit
               IMPORTING  iv_struc    TYPE struc
               RETURNING VALUE(r_res) TYPE char1
           , get_common_hasaward
               IMPORTING  iv_struc    TYPE struc
               RETURNING VALUE(r_res) TYPE char1
           , get_objid_struc
               IMPORTING i_objid      TYPE hrobjid
                         i_wegid      TYPE wegid
               RETURNING VALUE(rt_struc)    TYPE struc_t

           , delete_excluded_path
               IMPORTING is_struc     TYPE struc
               CHANGING  ct_struc     TYPE struc_t
           , get_excluded_objid
              IMPORTING i_plvar       TYPE hrp1000-plvar
              RETURNING VALUE(rt_objid) TYPE ty_t_excluded_objid
           , check_for_empty
           , check_for_duplicates " 03.09.2020
           , get_data_for_cache
           , get_education_old_3x
           , check_oe_has_free_oe IMPORTING iv_objid      TYPE objid
                                  RETURNING VALUE(rv_res) TYPE abap_bool
           , get_name_insti       IMPORTING iv_insti      TYPE insti
                                  RETURNING VALUE(rv_res) TYPE string
           , check_baseedu        IMPORTING it_p0022      TYPE p0022ar_tab
                                  RETURNING VALUE(rv_ok)  TYPE abap_bool
*} MAKAROVSV 23.09.2019
           .
    CLASS-METHODS:
        is_redun
          IMPORTING i_objid      TYPE hrobjid
                    i_begda      TYPE begda
                    i_endda      TYPE begda
          CHANGING  c_dateend    TYPE string
          RETURNING VALUE(r_yes) TYPE flag
      , is_vacancy
          IMPORTING i_objid      TYPE hrobjid
                    i_begda      TYPE begda
                    i_endda      TYPE begda
          RETURNING VALUE(r_yes) TYPE flag
      , read_subty_text
          IMPORTING i_subty TYPE subty
          RETURNING VALUE(r_str) TYPE string
      , read_wdgrd_text
          IMPORTING i_wdgrd TYPE wdgrd
          RETURNING VALUE(r_str) TYPE string
      , read_massg_text
          IMPORTING i_massn TYPE massn
                    i_massg TYPE massg
                    i_masss TYPE p33_masss
          RETURNING VALUE(r_str) TYPE string
      , get_abolitiondate
          IMPORTING i_objid           TYPE hrobjid
                    i_begda           TYPE begda
                    i_endda           TYPE begda
          RETURNING VALUE(r_begda)    TYPE string
      , get_citizenshipokin
          IMPORTING i_natio           TYPE natsl
          RETURNING VALUE(r_str)      TYPE string.

    TYPES: BEGIN OF ty_subty_cache,
             subty TYPE t554t-awart,
             text  TYPE t554t-atext,
           END OF ty_subty_cache.
    CLASS-DATA: mt_subty_cache TYPE HASHED TABLE OF ty_subty_cache WITH UNIQUE KEY subty.
    TYPES: BEGIN OF ty_wdgrd_cache,
             wdgrd TYPE t554d-wdgrd,
             text  TYPE t554d-zztext,
           END OF ty_wdgrd_cache.
    CLASS-DATA: mt_wdgrd_cache TYPE HASHED TABLE OF ty_wdgrd_cache WITH UNIQUE KEY wdgrd.

    TYPES: BEGIN OF ty_massg_cache,
             massn TYPE t530t-massn,
             massg TYPE t530t-massg,
             text  TYPE t530t-mgtxt,
           END OF ty_massg_cache.
    CLASS-DATA: mt_massg_cache TYPE HASHED TABLE OF ty_massg_cache WITH UNIQUE KEY massn massg.

    CLASS-DATA: mt_t7rurst_cache TYPE HASHED TABLE OF t7rurst WITH UNIQUE KEY sprsl massn massg masss.

    "Кеширование данных
    DATA:
        v_cache_foreignlanguage TYPE tvarvc-low
      , v_cache_attestation     TYPE tvarvc-low
      , t_cache_foreignlanguage TYPE HASHED TABLE OF t7ruokin WITH UNIQUE KEY primary_key COMPONENTS cname
      , t_cache_cacheid          TYPE HASHED TABLE OF zthr_ess_d001 WITH UNIQUE KEY primary_key COMPONENTS personid block block_type block_date
                                                                    WITH NON-UNIQUE SORTED KEY personid COMPONENTS personid
      , t_cache_relatives_kinshipokin TYPE HASHED TABLE OF t7ruokin WITH UNIQUE KEY primary_key COMPONENTS ccode
      , t_cache_certifications_reason TYPE HASHED TABLE OF t530t    WITH UNIQUE KEY primary_key COMPONENTS massg
      , t_cache_socialsecurity  TYPE HASHED TABLE OF t591s WITH UNIQUE KEY primary_key COMPONENTS infty subty
      , t_cache_education_3x TYPE RANGE OF p0022-zzshort
      , t_cache_military TYPE HASHED TABLE OF t7rum0 WITH UNIQUE KEY primary_key COMPONENTS partn cdpfl
      .
ENDCLASS.
CLASS lcl_ess_d001 IMPLEMENTATION.
  METHOD constructor.
    DATA: lt_roots TYPE TABLE OF hrrootob
        .
    zcl_hr_util=>sel_screen_fill_struc( EXPORTING iv_shift = 2 IMPORTING es_struc = ms_prop ).
*{ MAKAROVSV 16.09.2019
    IF ms_prop-begda[ 1 ]-low IS INITIAL.
      ms_prop-begda[ 1 ]-low = hr_low_date.
    ENDIF.
*} MAKAROVSV 16.09.2019
    ms_prop-otype = `O`.

    ms_prop-folder = iv_folder.

    "Получим вариант плана
    CALL FUNCTION 'RH_GET_ACTIVE_WF_PLVAR'
      IMPORTING
        act_plvar       = ms_prop-plvar
      EXCEPTIONS
        no_active_plvar = 1
        OTHERS          = 2.


    IF ms_prop-objid[] IS INITIAL AND ms_prop-pernr[] IS INITIAL.
      CALL FUNCTION 'RH_STRUC_HELP_ROOTS_GET'
        EXPORTING
          search_plvar  = zcl_hr_get_data=>a_plvar
          search_otype  = `O`
          search_begda  = ms_prop-begda[ 1 ]-high
          search_endda  = ms_prop-begda[ 1 ]-high
        TABLES
          roots         = lt_roots
        EXCEPTIONS
          no_stru_seark = 1
          OTHERS        = 2.

      LOOP AT lt_roots ASSIGNING FIELD-SYMBOL(<lt_roots>).
        APPEND VALUE #( low = <lt_roots>-objid ) TO ms_prop-objid.
      ENDLOOP.
    ELSEIF ms_prop-pernr[] IS NOT INITIAL AND ms_prop-objid[] IS INITIAL.
      SELECT DISTINCT pernr AS low FROM pa0000 INTO CORRESPONDING FIELDS OF TABLE ms_prop-objid[] WHERE pernr IN ms_prop-pernr[].
*      ms_prop-objid[] = ms_prop-pernr[].
      ms_prop-otype   = `P`.
    ENDIF.

    APPEND VALUE #( sign = 'I' option = 'EQ' low = '91' ) TO ms_prop-sub22.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '92' ) TO ms_prop-sub22.

    CREATE OBJECT mo_get_data.
  ENDMETHOD.
  METHOD get_data.
    DATA: lv_str_tmp      TYPE string
        , ls_plans_count  TYPE ty_plans_count
        , lv_redun        TYPE flag
        , ls_wegid        TYPE wegid "klokovnyu 23.05.2019
        .

    get_data_for_cache( ).

*{KlokovNYU 23.05.2019
    IF s_objid IS NOT INITIAL.
      ls_wegid = p_o_weg.
    ELSE.
      ls_wegid = p_p_weg.
    ENDIF.
*}KlokovNYU 23.05.2019

    mt_excluded_objid = get_excluded_objid( i_plvar = '01' ).

    LOOP AT ms_prop-objid ASSIGNING FIELD-SYMBOL(<objid>).

      DATA(lt_struc) = get_objid_struc(
        EXPORTING
          i_objid = <objid>-low
          i_wegid = ls_wegid
      ).

      IF ms_prop-orgeh = abap_true.
        "Выплним проверку ОЕ на содержание независимых ОЕ
        DATA:
            lr_obgid_first  TYPE RANGE OF objid
          , lr_obgid_second TYPE RANGE OF objid
          .
        CLEAR:
            lr_obgid_first
          , lr_obgid_second
          .

        DATA lv_flag_oe_has_free_oe TYPE abap_bool.
        CLEAR lv_flag_oe_has_free_oe.

        IF ls_wegid = p_o_weg.
          SORT lt_struc  BY level.
        ELSEIF ls_wegid = p_p_weg.
          SORT lt_struc BY level DESCENDING seqnr DESCENDING.
          DELETE lt_struc WHERE otype = 'O' AND objid = p_admin.
        ENDIF.
        READ TABLE lt_struc INDEX 1 ASSIGNING FIELD-SYMBOL(<lt_struc>).
        IF sy-subrc IS INITIAL.
          lv_flag_oe_has_free_oe = check_oe_has_free_oe(
            iv_objid = CONV #( <lt_struc>-objid )
          ).
          IF lv_flag_oe_has_free_oe = abap_true.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = <lt_struc>-objid ) TO lr_obgid_first.
            READ TABLE lt_struc INDEX 2 ASSIGNING <lt_struc>.
            IF sy-subrc IS INITIAL.
              DATA(lv_level) = <lt_struc>-level.
              LOOP AT lt_struc ASSIGNING <lt_struc> WHERE level = lv_level
                                                      AND otype = 'O'.
                APPEND VALUE #( sign = 'I' option = 'EQ' low = <lt_struc>-objid ) TO lr_obgid_second.

              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      DATA(lt_struc2) = lt_struc.

      SORT: lt_struc  BY seqnr
          , lt_struc2 BY pup otype.

      LOOP AT lt_struc ASSIGNING <lt_struc>.
        CASE <lt_struc>-otype.
          WHEN `O`.

            CHECK ms_prop-orgeh = abap_true.
            CLEAR: ms_organization
                 .

            "Уникальный идентификатор
            ms_organization-organizationid = <lt_struc>-objid.
            "Название подразделения
            ms_organization-name = mo_get_data->get_objid_verbal(
              i_otype = <lt_struc>-otype
              i_objid = CONV #( <lt_struc>-objid )
              i_begda = ms_prop-begda[ 1 ]-high
            ).
            "Дата создани
            "Код подразделения
            "Короткое название подразделения
            me->read_p1000_data(
              EXPORTING
                iv_otype = <lt_struc>-otype
                iv_objid = CONV #( <lt_struc>-objid )
              IMPORTING
                ev_datum = ms_organization-creationdate
                ev_code  = ms_organization-code
                ev_sname = ms_organization-shortname
            ).
            "Код вышестоящего подразделения
            IF ls_wegid = p_p_weg.
              "Если был использован «Путь анализа для ТН» для получения lt_struc, то в этом случае нужно искать не предыдущую запись, а следующую:
              READ TABLE lt_struc ASSIGNING FIELD-SYMBOL(<zt_struc>) WITH KEY seqnr = <lt_struc>-pdown BINARY SEARCH.
              IF sy-subrc = 0.
                ms_organization-parentid = <zt_struc>-objid.
              ELSE.
                ms_organization-parentid = '0'.
              ENDIF.
            ELSE.
              READ TABLE lt_struc ASSIGNING <zt_struc> WITH KEY seqnr = <lt_struc>-pup BINARY SEARCH. "<lt_struc>-pdown BINARY SEARCH. " MAKAROVSV 15.10.2019
              IF sy-subrc = 0.
                ms_organization-parentid = <zt_struc>-objid.
              ELSE.
                ms_organization-parentid = '0'. "get_upper_orgeh( i_objid = CONV #( <lt_struc>-objid ) ).
              ENDIF.
            ENDIF.
            ms_organization-hierarchyname = ms_organization-name.
            ms_organization-namegenitive = ''.
            ms_organization-namegenitive = mo_get_data->get_objid_verbal(
              i_subty = '9001'
              i_otype = <lt_struc>-otype
              i_objid = CONV #( <lt_struc>-objid )
              i_begda = ms_prop-begda[ 1 ]-high
            ).
*            IF ms_organization-namegenitive = ms_organization-shortname.
*              CLEAR ms_organization-namegenitive.
*            ENDIF.
            IF ms_organization-namegenitive IS INITIAL.
              ms_organization-namegenitive = ms_organization-name.
            ENDIF.
            ms_organization-namedative = ''.
            ms_organization-namedative = mo_get_data->get_objid_verbal(
              i_subty = '9002'
              i_otype = <lt_struc>-otype
              i_objid = CONV #( <lt_struc>-objid )
              i_begda = ms_prop-begda[ 1 ]-high
            ).
*            IF ms_organization-namedative = ms_organization-shortname.
*              CLEAR ms_organization-namedative.
*            ENDIF.
            IF ms_organization-namedative IS INITIAL.
              ms_organization-namedative = ms_organization-name.
            ENDIF.
            ms_organization-nameaccusative = ''.
            ms_organization-nameaccusative = mo_get_data->get_objid_verbal(
              i_subty = '9003'
              i_otype = <lt_struc>-otype
              i_objid = CONV #( <lt_struc>-objid )
              i_begda = ms_prop-begda[ 1 ]-high
            ).
*            IF ms_organization-nameaccusative = ms_organization-shortname.
*              CLEAR ms_organization-nameaccusative.
*            ENDIF.
            IF ms_organization-nameaccusative IS INITIAL.
              ms_organization-nameaccusative = ms_organization-name.
            ENDIF.

            ms_organization-abolitiondate = get_abolitiondate(
                  i_objid = CONV #( <lt_struc>-objid )
                  i_begda = ms_prop-begda[ 1 ]-low
                  i_endda = ms_prop-begda[ 1 ]-high
            ).

            IF lv_flag_oe_has_free_oe = abap_true.
              "Добавляем обработку в случае если ОЕ содержит независимые ОЕ
              IF <lt_struc>-objid IN lr_obgid_first.
                "Для первых меняем поля
                ms_organization-name            = |Руководство|.
                ms_organization-shortname       = |Руководство|.
                ms_organization-hierarchyname   = |Руководство|.
                ms_organization-namegenitive    = |Руководство|.
                ms_organization-namedative      = |Руководство|.
                ms_organization-nameaccusative  = |Руководство|.
                ms_organization-parentid        = |0|.
              ENDIF.
              IF <lt_struc>-objid IN lr_obgid_second.
                "Для вторых обнуляем родителя
                ms_organization-parentid        = |0|.
              ENDIF.
            ENDIF.

            COLLECT ms_organization INTO ms_data-organization.

          WHEN `S`.

            DATA lv_string_dataend TYPE string.
            CLEAR lv_string_dataend.

            IF is_vacancy( i_objid = CONV #( <lt_struc>-objid )
                           i_begda = ms_prop-begda[ 1 ]-high                  "MAKAROVSV 16.09.2019
                           i_endda = ms_prop-begda[ 1 ]-high ) EQ abap_true.  "MAKAROVSV 16.09.2019
              ADD 1 TO ms_data-header-vacancynumbers.
            ENDIF.
            lv_redun = is_redun(
              EXPORTING
                  i_objid = CONV #( <lt_struc>-objid )
                  i_begda = ms_prop-begda[ 1 ]-high   "MAKAROVSV 16.09.2019
                  i_endda = ms_prop-begda[ 1 ]-high
              CHANGING
                  c_dateend  = lv_string_dataend"MAKAROVSV 16.09.2019
            ).
*           алгоритм из ZHR_OM_D005 поле ИТОГО: штатки не администрации  исключая устаревшие
            READ TABLE lt_struc ASSIGNING <zt_struc>
              WITH KEY seqnr = <lt_struc>-pup BINARY SEARCH.
            IF sy-subrc EQ 0 AND
                <zt_struc>-objid NE p_admin.
              IF lv_redun EQ abap_false.
                ADD 1 TO ms_data-header-staffnumbers.
              ENDIF.
            ENDIF.

*           алгоритм программы ZHR_STAFFLIST2 поле ИТОГО
            IF lv_redun EQ abap_false.
              ls_plans_count-plans = <lt_struc>-objid.
              COLLECT ls_plans_count INTO mt_plans_count.
            ENDIF.

            CHECK ms_prop-plans = abap_true.
            CLEAR: ms_staffunit
                 .
            "Уникальный идентификатор
            ms_staffunit-staffunitid    = <lt_struc>-objid.

            "Идентификатор подразделения
            IF p_p_weg = ls_wegid.
              READ TABLE lt_struc2 ASSIGNING FIELD-SYMBOL(<lt_struc2>) WITH KEY pup   = <lt_struc>-seqnr otype = `O`.
              IF sy-subrc = 0 AND <lt_struc2> IS ASSIGNED.
                ms_staffunit-organizationid  = <lt_struc2>-objid.
              ENDIF.
            ELSE.
              READ TABLE lt_struc ASSIGNING <zt_struc> WITH KEY seqnr = <lt_struc>-pup BINARY SEARCH.
              IF sy-subrc = 0 AND <zt_struc> IS ASSIGNED.
                ms_staffunit-organizationid  = <zt_struc>-objid.
              ENDIF.
            ENDIF.

            "Идентификатор должности
            READ TABLE lt_struc2 ASSIGNING <lt_struc2> WITH KEY pup   = <lt_struc>-seqnr otype = `C`.
            IF sy-subrc = 0 AND <lt_struc2> IS ASSIGNED.
              ms_staffunit-positionid	    = <lt_struc2>-objid.
            ENDIF.

            me->read_p1000_data( EXPORTING iv_otype = <lt_struc>-otype
                                           iv_objid = CONV #( <lt_struc>-objid )
                                 IMPORTING ev_datum = ms_staffunit-datestart  "Дата создания
                               ).
            ms_staffunit-dateend = lv_string_dataend.

            COLLECT ms_staffunit INTO ms_data-staffunit.
          WHEN `C`."Должности

            CHECK ms_prop-stell = abap_true.
            CLEAR: ms_position
                 .
            "Уникальный идентификатор
            ms_position-positionid = <lt_struc>-objid.

            me->read_p1000_data( EXPORTING iv_otype = <lt_struc>-otype
                                           iv_objid = CONV #( <lt_struc>-objid )
                                 IMPORTING ev_datum = ms_position-datestart    "Дата начала действия
                                           ev_code  = ms_position-code         "Код должности
                                           ev_sname = ms_position-shortname    "Короткое название должности
                                ).
            "Название должности
            ms_position-name = mo_get_data->get_objid_verbal( i_otype = <lt_struc>-otype i_objid = CONV #( <lt_struc>-objid ) i_begda = ms_prop-begda[ 1 ]-high ). "MAKAROVSV 16.09.2019
            "Категория должности + Группа должности
            mo_get_data->get_objid_struc( EXPORTING i_otype = <lt_struc>-otype
                                                    i_objid = CONV #( <lt_struc>-objid )
                                                    i_begda = ms_prop-begda[ 1 ]-high
                                                    i_wegid = 'ZSC2G2K'
                                          IMPORTING e_struc = DATA(yt_struc) ).

            LOOP AT yt_struc ASSIGNING FIELD-SYMBOL(<yt_struc>) WHERE otype = `2G` OR otype = `2K`.
              lv_str_tmp = mo_get_data->get_objid_verbal( i_otype = <yt_struc>-otype i_objid = CONV #( <yt_struc>-objid ) i_begda = ms_prop-begda[ 1 ]-high ). "MAKAROVSV 16.09.2019
              CASE <yt_struc>-otype.
                WHEN `2G`.
                  "Группа должности
                  SPLIT lv_str_tmp AT space INTO ms_position-groupname DATA(lv_dummy).
                WHEN `2K`.
                  "Категория должности
                  ms_position-categoryname = lv_str_tmp.
              ENDCASE.
            ENDLOOP.

*{ MAAKROV 16.09.2019
            "Признак государственной должности
            CASE ms_position-code+4(1).
              WHEN '1'.
                ms_position-isstateposition = `3`.
              WHEN '2'.
                ms_position-isstateposition = `1`.
              WHEN OTHERS.
                ms_position-isstateposition = `0`.
            ENDCASE.
*} MAKAROVSV 16.09.2019

***            Нужно изменить алгоритм и выводить всегда 0
***            << add Shibkova 19.03.2018
**            LOOP AT yt_struc TRANSPORTING NO FIELDS WHERE otype = `2K`
**                                                      AND objid = `50010679`.
**              ms_position-isstateposition = `1`.
**            ENDLOOP.
***            >> end
            "Значение оклада

*{ KLOKOVNYU 17.06.2019
            ms_position-namegenitive = mo_get_data->get_objid_verbal(
              i_subty = '9001'
              i_otype = <lt_struc>-otype
              i_objid = CONV #( <lt_struc>-objid )
              i_begda = ms_prop-begda[ 1 ]-high
            ).
            IF ms_position-namegenitive IS INITIAL.
              ms_position-namegenitive = ms_position-name.
            ENDIF.
*            IF ms_position-namegenitive = ms_position-shortname.
*              CLEAR ms_position-namegenitive.
*            ENDIF.

            ms_position-namedative = mo_get_data->get_objid_verbal(
              i_subty = '9002'
              i_otype = <lt_struc>-otype
              i_objid = CONV #( <lt_struc>-objid )
              i_begda = ms_prop-begda[ 1 ]-high
            ).
            IF ms_position-namedative IS INITIAL.
              ms_position-namedative = ms_position-name.
            ENDIF.
*            IF ms_position-namedative = ms_position-shortname.
*              CLEAR ms_position-namedative.
*            ENDIF.

            ms_position-nameaccusative = mo_get_data->get_objid_verbal(
              i_subty = '9003'
              i_otype = <lt_struc>-otype
              i_objid = CONV #( <lt_struc>-objid )
              i_begda = ms_prop-begda[ 1 ]-high
            ).
            IF ms_position-nameaccusative IS INITIAL.
              ms_position-nameaccusative = ms_position-name.
            ENDIF.
*            IF ms_position-nameaccusative = ms_position-shortname.
*              CLEAR ms_position-nameaccusative.
*            ENDIF.
*} KLOKOVNYU 17.06.2019

            me->read_p1005_data( EXPORTING iv_otype = <lt_struc>-otype
                                           iv_objid = CONV #( <lt_struc>-objid )
                                 IMPORTING ev_cpmin = ms_position-postsalary ).
            COLLECT ms_position INTO ms_data-position.
          WHEN `P`.
            CHECK <lt_struc>-objid IN ms_prop-pernr.  "MAKAROVSV 16.09.2019

            me->read_p0000( iv_pernr = CONV #( <lt_struc>-objid ) ). " 05/06/2020
            DATA(mv_dismissed) = abap_false.

            CASE ms_p0000-stat2.   " 05/06/2020
              WHEN '0'. " уволен
                mv_cdate = ms_p0000-begda - 1.
                mv_dismissed = abap_true.
              WHEN OTHERS.
                mv_cdate = ms_prop-begda[ 1 ]-high.
            ENDCASE.

            me->read_p0001( iv_pernr = CONV #( <lt_struc>-objid ) ).
*           Отчет ZHR_STAFFLIST_EXT_01 lv_sum_dekr_main декрет
*            IF ms_p0001-persg eq '3'.
*            табельники, по идее и декретницы тут же должны быть
            ADD 1 TO ms_data-header-currentnumbers.
*            ENDIF.
            "Воинский учет
            me->fill_military( iv_struc = <lt_struc> ).
            "Образование
            me->fill_education( iv_struc = <lt_struc> ).
            "Трудовая деятельность в государственном органе
            me->fill_orderappointment( iv_struc = <lt_struc> ).
            "Трудовая должность до замещения должности в государственном органе
            me->fill_workactivitie( iv_struc = <lt_struc> ).
*           Классный чин, воинское звание
            fill_orderranks( iv_struc = <lt_struc> ).
*           Сведения об окладе и надбавках
            fill_ordersalary( iv_struc = <lt_struc> ).
*           Состав семьи
            fill_relatives( iv_struc = <lt_struc> ).
*           Повышение квалификации, Профессиональная переподготовка
            fill_educationadditionals( iv_struc = <lt_struc> ).
*           Аттестация
            fill_certifications( iv_struc = <lt_struc> ).
*           Отпуск
            fill_holidays( iv_struc = <lt_struc> ).

            IF ms_p0000-stat2 <> '3'.
*           Сведения об увольнении
              fill_orderdismissals( iv_struc = <lt_struc> ).
            ENDIF.

*{ KLOKOVNYU 19.06.2019
            fill_education_grad( iv_struc = <lt_struc> ).
            fill_orderaward( iv_struc = <lt_struc> ).
*} KLOKOVNYU 19.06.2019
*{ MAKAROVSV 20.09.2019
            "Знание иностранного языка
            fill_foreignlanguage( iv_struc = <lt_struc> ).
            "Социальные льготы
            fill_socialsecurity( iv_struc = <lt_struc> ).
            "Общие сведения
            fill_common( iv_struc = <lt_struc> ).
*} MAKAROVSV 20.09.2019
          WHEN OTHERS.
            "
        ENDCASE.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD end.
    DATA: lt_xml    TYPE swxmlcont
        , lv_return TYPE text1000
        .
    ms_data-prop = ms_prop.

    fill_header( ).

    " 03.09.2020   <
    check_for_duplicates( ).
    " 03.09.2020   >

    check_for_empty( ).

    TRY.
        CALL TRANSFORMATION zhr_ess_d001
         SOURCE root = ms_data
         RESULT XML lt_xml.
      CATCH cx_st_error.
        RETURN.
    ENDTRY.

    IF ms_prop-folder IS INITIAL.
      CALL FUNCTION 'GUI_GET_DESKTOP_INFO'
        EXPORTING
          type   = 4  "TmpDir
        CHANGING
          return = lv_return
        EXCEPTIONS
          OTHERS = 99.

      IF lv_return IS INITIAL.
        lv_return = 'C:\TEMP'.
      ENDIF.

      ms_prop-folder = lv_return.
    ENDIF.

    DATA(lv_filename) = CONV string( ms_prop-folder && `\` && sy-repid && `_` && sy-datum && `_` && sy-uzeit && `.xml` ).

    cl_gui_frontend_services=>gui_download( EXPORTING  filename         = lv_filename
                                                       filetype         = 'BIN'
                                            CHANGING   data_tab         = lt_xml
                                            EXCEPTIONS file_write_error = 1
                                                       OTHERS           = 99 ).
  ENDMETHOD.

  METHOD check_for_empty.
    IF ms_data-organization IS INITIAL.
      ms_data-orgeh = abap_false.
    ENDIF.

    IF ms_data-position IS INITIAL.
      ms_data-stell = abap_false.
    ENDIF.

    IF ms_data-staffunit IS INITIAL.
      ms_data-plans = abap_false.
    ENDIF.

    IF ms_data-common IS INITIAL.
      ms_data-per00 = abap_false.
    ENDIF.

    IF ms_data-military IS INITIAL.
      ms_data-per01 = abap_false.
    ENDIF.

    IF ms_data-education IS INITIAL.
      ms_data-per02 = abap_false.
    ENDIF.

    IF ms_data-orderappointment IS INITIAL.
      ms_data-per03 = abap_false.
    ENDIF.

    IF ms_data-workactivitie IS INITIAL.
      ms_data-per04 = abap_false.
    ENDIF.

    IF ms_data-ordersalary IS INITIAL.
      ms_data-per05 = abap_false.
    ENDIF.

    IF ms_data-relatives IS INITIAL.
      ms_data-per06 = abap_false.
    ENDIF.

    IF ms_data-certifications IS INITIAL.
      ms_data-per08 = abap_false.
    ENDIF.

    IF ms_data-educationadditional IS INITIAL.
      ms_data-per07 = abap_false.
    ENDIF.

    IF ms_data-holidays IS INITIAL.
      ms_data-per09 = abap_false.
    ENDIF.

    IF ms_data-orderranks IS INITIAL.
      ms_data-per10 = abap_false.
    ENDIF.

    IF ms_data-orderdismissals IS INITIAL.
      ms_data-per11 = abap_false.
    ENDIF.

    IF ms_data-foreignlanguage IS INITIAL.
      ms_data-per12 = abap_false.
    ENDIF.

    IF ms_data-education_grad IS INITIAL.
      ms_data-per13 = abap_false.
    ENDIF.

    IF ms_data-orderaward IS INITIAL.
      ms_data-per14 = abap_false.
    ENDIF.

    IF ms_data-socialsecurity IS INITIAL.
      ms_data-per15 = abap_false.
    ENDIF.

  ENDMETHOD.

  " 03.09.2020   <
  METHOD check_for_duplicates.

    IF ms_data-organization IS NOT INITIAL.
      SORT ms_data-organization.
      DELETE ADJACENT DUPLICATES FROM ms_data-organization COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-position IS NOT INITIAL.
      SORT ms_data-position.
      DELETE ADJACENT DUPLICATES FROM ms_data-position COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-staffunit IS NOT INITIAL.
      SORT ms_data-staffunit.
      DELETE ADJACENT DUPLICATES FROM ms_data-staffunit COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-common IS NOT INITIAL.
      SORT ms_data-common.
      DELETE ADJACENT DUPLICATES FROM ms_data-common COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-military IS NOT INITIAL.
      SORT ms_data-military.
      DELETE ADJACENT DUPLICATES FROM ms_data-military COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-education IS NOT INITIAL.
      SORT ms_data-education.
      DELETE ADJACENT DUPLICATES FROM ms_data-education COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-orderappointment IS NOT INITIAL.
      SORT ms_data-orderappointment.
      DELETE ADJACENT DUPLICATES FROM ms_data-orderappointment COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-workactivitie IS NOT INITIAL.
      SORT ms_data-workactivitie.
      DELETE ADJACENT DUPLICATES FROM ms_data-workactivitie COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-ordersalary IS NOT INITIAL.
      SORT ms_data-ordersalary.
      DELETE ADJACENT DUPLICATES FROM ms_data-ordersalary COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-relatives IS NOT INITIAL.
      SORT ms_data-relatives.
      DELETE ADJACENT DUPLICATES FROM ms_data-relatives COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-certifications IS NOT INITIAL.
      SORT ms_data-certifications.
      DELETE ADJACENT DUPLICATES FROM ms_data-certifications COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-educationadditional IS NOT INITIAL.
      SORT ms_data-educationadditional.
      DELETE ADJACENT DUPLICATES FROM ms_data-educationadditional COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-holidays IS NOT INITIAL.
      SORT ms_data-holidays.
      DELETE ADJACENT DUPLICATES FROM ms_data-holidays COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-orderranks IS NOT INITIAL.
      SORT ms_data-orderranks.
      DELETE ADJACENT DUPLICATES FROM ms_data-orderranks COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-orderdismissals IS NOT INITIAL.
      SORT ms_data-orderdismissals.
      DELETE ADJACENT DUPLICATES FROM ms_data-orderdismissals COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-foreignlanguage IS NOT INITIAL.
      SORT ms_data-foreignlanguage.
      DELETE ADJACENT DUPLICATES FROM ms_data-foreignlanguage COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-education_grad IS NOT INITIAL.
      SORT ms_data-education_grad.
      DELETE ADJACENT DUPLICATES FROM ms_data-education_grad COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-orderaward IS NOT INITIAL.
      SORT ms_data-orderaward.
      DELETE ADJACENT DUPLICATES FROM ms_data-orderaward COMPARING ALL FIELDS.
    ENDIF.

    IF ms_data-socialsecurity IS NOT INITIAL.
      SORT ms_data-socialsecurity.
      DELETE ADJACENT DUPLICATES FROM ms_data-socialsecurity COMPARING ALL FIELDS.
    ENDIF.

  ENDMETHOD.
  " 03.09.2020   >

  METHOD read_p0000.
    DATA: lt_p0000 TYPE TABLE OF p0000
        .

    IF ms_p0001-pernr EQ iv_pernr.
      RETURN.
    ENDIF.

    mo_get_data->read_pa_infty(
      EXPORTING i_pernr = iv_pernr
                i_begda = ms_prop-begda[ 1 ]-high
                i_endda = ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                i_infty = `0000`
      IMPORTING e_pnnnn = lt_p0000 ).

    READ TABLE lt_p0000 INTO ms_p0000 INDEX 1.
  ENDMETHOD.

  METHOD read_p0001.
    DATA: lt_p0001 TYPE TABLE OF p0001
        .

    IF ms_p0001-pernr EQ iv_pernr.
      RETURN.
    ENDIF.

    CLEAR: ms_p0001.
    mo_get_data->read_pa_infty(
      EXPORTING i_pernr = iv_pernr
                i_endda = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                i_infty = `0001`
      IMPORTING e_pnnnn = lt_p0001 ).

    SORT lt_p0001 BY begda DESCENDING.
    READ TABLE lt_p0001 INTO ms_p0001 INDEX 1.
  ENDMETHOD.
  METHOD fill_common.
    DATA: lt_p0002    TYPE TABLE OF p0002
        , lt_p0016    TYPE TABLE OF p0016
        , lt_p0290    TYPE TABLE OF p0290
        , lt_p0001    TYPE TABLE OF p0001
        , lt_p0006    TYPE TABLE OF p0006 "KLOKOVNYU 27.05.2019
        , lt_p0081    TYPE TABLE OF p0081 "KLOKOVNYU 17.06.2019
        , lt_p0021    TYPE TABLE OF p0021 "KLOKOVNYU 19.06.2019
        , lt_p0105    TYPE TABLE OF p0105 "MAKAROVSV 19.09.2019
        , lv_datum TYPE text10
        .
    CHECK ms_prop-per00 = abap_true.
    CLEAR: ms_common
         .
    "Идентификатор сотрудника
    ms_common-personid                    = iv_struc-objid.
    mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
                                          i_begda         = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                          i_endda         = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                          i_infty         = '0002'
                                IMPORTING e_pnnnn         = lt_p0002 ).
    LOOP AT lt_p0002 ASSIGNING FIELD-SYMBOL(<lt_p0002>).
      ms_common-lastname    = <lt_p0002>-nachn."Фамилия
      ms_common-firstname  	= <lt_p0002>-vorna."Имя
      ms_common-middlename  = <lt_p0002>-midnm."Отчество
*      ms_common-dateofbirth  = <lt_p0002>-gbdat."Дата рождения
      WRITE <lt_p0002>-gbdat TO lv_datum LEFT-JUSTIFIED.
      ms_common-dateofbirth = lv_datum ."Дата рождения
      ms_common-gender      = <lt_p0002>-gesch."Пол
*{ MAKAROVSV 19.09.2019
      ms_common-citizenshipokin = get_citizenshipokin( i_natio = <lt_p0002>-natio ).
*} MAKAROVSV 19.09.2019
      CASE <lt_p0002>-gesch.
        WHEN '1'.
          ms_common-gender = 'Мужской'(005).
        WHEN '2'.
          ms_common-gender = 'Женский'(006).
      ENDCASE.

      ms_common-maritalstatusokin = <lt_p0002>-famst. " нужен код

      READ TABLE mt_t502t ASSIGNING FIELD-SYMBOL(<mt_t502t>) WITH TABLE KEY famst = <lt_p0002>-famst.
      IF sy-subrc = 0 AND <mt_t502t> IS ASSIGNED.
        ms_common-maritalstatusokin = <mt_t502t>-zz_text_okin. "Состояние в браке
      ENDIF.
    ENDLOOP.

    mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
                                          i_begda         = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                          i_endda         = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                          i_infty         = '0290'
                                IMPORTING e_pnnnn         = lt_p0290 ).
    LOOP AT lt_p0290 ASSIGNING FIELD-SYMBOL(<lt_p0290>).
      CASE <lt_p0290>-subty.
        WHEN `801`.
          ms_common-taxcertificatenumber        = <lt_p0290>-nomer."Идентификационный номер налогоплательщика
        WHEN `802`.
          ms_common-insurancecertificatenumber  = <lt_p0290>-nomer."Номер страхового свидетельства государственного пенсионного страхования
*{ MAKAROVSV 19.09.2019
          DATA lv_coma TYPE char2 VALUE `, `.
          IF <lt_p0290>-mstrana IS NOT INITIAL.
            CONCATENATE ms_common-birthplace <lt_p0290>-mstrana lv_coma INTO ms_common-birthplace.
          ENDIF.
          IF <lt_p0290>-mregion IS NOT INITIAL.
            CONCATENATE ms_common-birthplace <lt_p0290>-mregion lv_coma INTO ms_common-birthplace.
          ENDIF.
          IF <lt_p0290>-mraion IS NOT INITIAL.
            CONCATENATE ms_common-birthplace <lt_p0290>-mraion lv_coma INTO ms_common-birthplace.
          ENDIF.
          IF <lt_p0290>-mplace IS NOT INITIAL.
            CONCATENATE ms_common-birthplace <lt_p0290>-mplace lv_coma INTO ms_common-birthplace.
          ENDIF.
          DATA(lv_strlen) = strlen( ms_common-birthplace ).
          IF lv_strlen IS NOT INITIAL.
            lv_strlen = lv_strlen - 1.
            ms_common-birthplace = ms_common-birthplace(lv_strlen).
            REPLACE ALL OCCURRENCES OF ',' IN ms_common-birthplace WITH `, `.
          ENDIF.
        WHEN `803`.
          ms_common-medicalinsurancepolicynumber = <lt_p0290>-text1(16) .
*} MAKAROVSV 19.09.2019
        WHEN `21`.
          ms_common-passportseries              = <lt_p0290>-seria && <lt_p0290>-seri0."Паспорт серия
          ms_common-passportnumber              = <lt_p0290>-nomer.                    "Паспорт номер
*          ms_common-passportissued              = <lt_p0290>-datbg.                   "Паспорт дата выдачи
          ms_common-passportissuedcode          = <lt_p0290>-pcode.                    "KLOKOVNYU 27.05.2019{
*{ KLOKOVNYU 04.07.2019
          REPLACE ALL OCCURRENCES OF |-| IN ms_common-passportissuedcode WITH ||.
*} KLOKOVNYU 04.07.2019
          CLEAR: ms_common-passportissued.
          IF <lt_p0290>-datbg IS NOT INITIAL.
            WRITE <lt_p0290>-datbg TO lv_datum LEFT-JUSTIFIED.
            ms_common-passportissued              = lv_datum.
          ENDIF.
          ms_common-passportissuedby            = |{ <lt_p0290>-passl }{ <lt_p0290>-passl2 }|. "Паспорт кем выдан
          CONDENSE: ms_common-passportissuedby.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    IF ms_common-birthplace IS INITIAL.
      "Так как не нашли место рождения ввыше пытаемся взять из 0002 ИТ
      READ TABLE lt_p0002 ASSIGNING FIELD-SYMBOL(<ls_p0002>) INDEX 1.
      IF sy-subrc IS INITIAL.
        ms_common-birthplace = <ls_p0002>-gbort.
      ENDIF.
    ENDIF.

    IF ms_common-passportissuedcode IS INITIAL.
      ms_common-passportissuedcode =  '000000'.
    ENDIF.
    IF ms_common-medicalinsurancepolicynumber IS INITIAL.
      "ms_common-medicalinsurancepolicynumber = 'значение не найдено'.
      ms_common-medicalinsurancepolicynumber = '1111111111111111'.
    ENDIF.
    REPLACE ALL OCCURRENCES OF '-' IN ms_common-taxcertificatenumber WITH ''.
    REPLACE ALL OCCURRENCES OF '-' IN ms_common-insurancecertificatenumber WITH ''.
    CONDENSE: ms_common-taxcertificatenumber NO-GAPS
            , ms_common-insurancecertificatenumber NO-GAPS .

    "Общий стаж
    DATA(ls_durat) = me->sen_calculate( iv_pernr = CONV #( iv_struc-objid ) iv_begda = mv_cdate iv_proce = `RU01` ).
    ms_common-experiencegeneralyear        = CONV int4( ls_durat-calyy )."лет (до)
    ms_common-experiencegeneralmonths	     = CONV int4( ls_durat-calmm )."месяцев (до)
    ms_common-experiencegeneraldays	       = CONV int4( ls_durat-caldd )."дней (до)
    "Стаж непрерывеый
    ms_common-experiencecontinuousyear     = ms_common-experiencegeneralyear.
    ms_common-experiencecontinuousmonths   = ms_common-experiencegeneralmonths.
    ms_common-experiencecontinuousdays     = ms_common-experiencegeneraldays.
    "Стаж государственной (муниципальной) службы
    DATA(ls_durat_zu04) = me->sen_calculate( iv_pernr = CONV #( iv_struc-objid ) iv_begda = mv_cdate iv_proce = `ZU04` ).
    DATA(ls_durat_zu03) = me->sen_calculate( iv_pernr = CONV #( iv_struc-objid ) iv_begda = mv_cdate iv_proce = `ZU03` ).
    DATA(ls_summ_experiens) = summ_experiens( is_exp1 = ls_durat_zu04 is_exp2 = ls_durat_zu03 ).
    ms_common-experiencestateyear	   = CONV int4( ls_summ_experiens-calyy )."Стаж государственной (муниципальной) службы лет (до)
    ms_common-experiencestatelmonths = CONV int4( ls_summ_experiens-calmm )."Стаж государственной (муниципальной) службы месяцев (до)
    ms_common-experiencestatedays	   = CONV int4( ls_summ_experiens-caldd )."Стаж государственной (муниципальной) службы дней (до)
    CONDENSE: ms_common-experiencegeneralyear NO-GAPS
            , ms_common-experiencegeneralmonths NO-GAPS
            , ms_common-experiencegeneraldays   NO-GAPS
            , ms_common-experiencecontinuousyear    NO-GAPS
            , ms_common-experiencecontinuousmonths  NO-GAPS
            , ms_common-experiencecontinuousdays    NO-GAPS
            , ms_common-experiencestateyear         NO-GAPS
            , ms_common-experiencestatelmonths      NO-GAPS
            , ms_common-experiencestatedays         NO-GAPS
            .
    "Табельный номер
    ms_common-personaltabelnumber         = iv_struc-objid.

    mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
                                          i_begda         = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                          i_endda         = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                          i_infty         = `0016`
                                IMPORTING e_pnnnn         = lt_p0016 ).
    LOOP AT lt_p0016 ASSIGNING FIELD-SYMBOL(<lt_p0016>).
      ms_common-numservicecontract          = <lt_p0016>-ctnum."Номер трудового договора (контракт)
*      ms_common-dateservicecontract          = <lt_p0016>-ctbeg."Дата трудового договора (контракт)
      IF <lt_p0016>-ctbeg IS NOT INITIAL.
        WRITE <lt_p0016>-ctbeg TO lv_datum LEFT-JUSTIFIED.
      ELSE.
        WRITE <lt_p0016>-begda TO lv_datum LEFT-JUSTIFIED.
      ENDIF.
      ms_common-dateservicecontract = lv_datum."Дата трудового договора (контракт)
    ENDLOOP.

*   этих полей может не быть в 16 ИТ
*   но они обязательные
*   поэтому если дату не нашли, то берем последнюю бегду из орг присвоения 0001 ИТ
*   <NumServiceContract> ставим  значение
*   бн
    IF ms_common-numservicecontract IS INITIAL.
      ms_common-numservicecontract = 'бн'.
    ENDIF.

    IF ms_common-dateservicecontract IS INITIAL.
      IF ms_p0001-begda IS INITIAL.
        mo_get_data->read_pa_infty(
          EXPORTING i_pernr = CONV #( iv_struc-objid )
                    i_begda         = hr_low_date
                    i_endda         = ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                    i_infty         = '0001'
          IMPORTING e_pnnnn         = lt_p0001 ).
        SORT lt_p0001 BY begda DESCENDING.
        READ TABLE lt_p0001 INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p0001>).
        IF sy-subrc EQ 0.
          ms_common-dateservicecontract = |{ <fs_p0001>-begda DATE = USER }|.
        ENDIF.
      ELSE.
        ms_common-dateservicecontract = |{ ms_p0001-begda DATE = USER }|.
      ENDIF.
    ENDIF.

    DATA: lo_struc  TYPE REF TO cl_abap_structdescr
        , lt_comps  TYPE cl_abap_structdescr=>component_table
        .
    FIELD-SYMBOLS: <fs_fld> TYPE any
                 .
*{ MAKAROVSV 11.11.2019
*    lo_struc ?= cl_abap_structdescr=>describe_by_data( p_data = ms_common ).
*    lt_comps = lo_struc->get_components( ).
*    LOOP AT lt_comps ASSIGNING FIELD-SYMBOL(<fs_comp>).
*      CASE <fs_comp>-name.
*        WHEN 'MIDDLENAME'.  " необязательное
*        WHEN OTHERS.
*          ASSIGN COMPONENT <fs_comp>-name OF STRUCTURE ms_common TO <fs_fld>.
*          CHECK sy-subrc EQ 0.
*          check_mandatory_fld( CHANGING cv_fld = <fs_fld> ).
*      ENDCASE.
*    ENDLOOP.
*} MAKAROVSV 11.11.2019

    format_pernr( CHANGING cv_fld = ms_common-personid ).
    format_pernr( CHANGING cv_fld = ms_common-personaltabelnumber ).
*{ KLOKOVNYU 17.06.2019
    mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
                                      i_begda         = ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                      i_endda         = ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                      i_infty         = '0006'
                            IMPORTING e_pnnnn         = lt_p0006 ).
*{ MAKAROVSV 19.09.2019
    READ TABLE lt_p0006 INTO DATA(ls_p0006) WITH KEY anssa = '1'.
    IF sy-subrc = 0.
      ms_common-addressofregistration = |г.{ ls_p0006-ort01 }, ул. { ls_p0006-stras } { ls_p0006-hsnmr }, кв.{ ls_p0006-posta }|.
    ELSE.
      ms_common-addressofregistration = ''.
    ENDIF.
    LOOP AT lt_p0006 INTO ls_p0006.
      CASE ls_p0006-subty.
        WHEN '1'.
          ms_common-addressofregistrationindex = ls_p0006-pstlz.
        WHEN '2'.
          IF ls_p0006-ort01 IS INITIAL AND
             ls_p0006-stras IS INITIAL AND
             ls_p0006-hsnmr IS INITIAL AND
             ls_p0006-posta IS INITIAL .
            "ms_common-addressofresidenceindex = ms_common-addressofregistrationindex.
            ms_common-addressofresidence      = ms_common-addressofregistration.
          ELSE.
            "ms_common-addressofresidenceindex = ls_p0006-pstlz.
            ms_common-addressofresidence      = |г.{ ls_p0006-ort01 }, ул. {  ls_p0006-stras } { ls_p0006-hsnmr }, кв. { ls_p0006-posta }|.
          ENDIF.
          ms_common-addressofresidenceindex = ls_p0006-pstlz.
      ENDCASE.
    ENDLOOP.
    mo_get_data->read_pa_infty(
      EXPORTING i_pernr = CONV #( iv_struc-objid )
        i_begda = ms_prop-begda[ 1 ]-high
        i_endda = ms_prop-begda[ 1 ]-high
        i_infty = `0105`
      IMPORTING
        e_pnnnn = lt_p0105
    ).
    ms_common-workphone = VALUE string( lt_p0105[ subty = '0004' ]-usrid_long OPTIONAL ).
*    DATA(lv_str) = VALUE string( lt_p0105[ subty = 'CELL' ]-usrid_long OPTIONAL ).
*    REPLACE ALL OCCURRENCES OF '+' IN lv_str WITH ''.
*    REPLACE ALL OCCURRENCES OF '(' IN lv_str WITH ''.
*    REPLACE ALL OCCURRENCES OF ')' IN lv_str WITH ''.
*    REPLACE ALL OCCURRENCES OF '-' IN lv_str WITH ''.
*    CONDENSE lv_str NO-GAPS.
*    lv_strlen = numofchar( lv_str ).
*    IF lv_strlen < 10.
*      ms_common-workphone = ''.
*    ELSE.
*      DATA lv_str14 TYPE char14.
*      lv_strlen = lv_strlen - 10.
*      WRITE lv_str+lv_strlen(10) USING EDIT MASK '+7 ___ _______' TO lv_str14.
*      ms_common-workphone = lv_str14.
*    ENDIF.

    ms_common-personaltabelnumber    = ms_common-personid.

    "Заполняем HAS* поля
    ms_common-hasrelative = 0.
    IF ms_prop-per06 = abap_true.
      READ TABLE ms_data-relatives WITH KEY personid = iv_struc-objid TRANSPORTING NO FIELDS .
      IF sy-subrc IS INITIAL.
        ms_common-hasrelative = 1.
      ENDIF.
    ENDIF.
    ms_common-hasadditionaleducation = 0.
    IF ms_prop-per07 = abap_true.
      READ TABLE ms_data-educationadditional WITH KEY personid = iv_struc-objid TRANSPORTING NO FIELDS .
      IF sy-subrc IS INITIAL.
        ms_common-hasadditionaleducation = 1.
      ENDIF.
    ENDIF.
    ms_common-hascertification = 0.
    IF ms_prop-per08 = abap_true.
      READ TABLE ms_data-certifications WITH KEY personid = iv_struc-objid TRANSPORTING NO FIELDS .
      IF sy-subrc IS INITIAL.
        ms_common-hascertification = 1.
      ENDIF.
    ENDIF.
    ms_common-hasrank = 0.
    IF ms_prop-per10 = abap_true.
      READ TABLE ms_data-orderranks WITH KEY personid = iv_struc-objid TRANSPORTING NO FIELDS .
      IF sy-subrc IS INITIAL.
        ms_common-hasrank = 1.
      ENDIF.
    ENDIF.
    ms_common-hasaward = 0.
    IF ms_prop-per14 = abap_true.
      READ TABLE ms_data-orderaward WITH KEY personid = iv_struc-objid TRANSPORTING NO FIELDS .
      IF sy-subrc IS INITIAL.
        ms_common-hasaward = 1.
      ENDIF.
    ENDIF.
    ms_common-hassocialbenefit = 0.
    IF ms_prop-per15 = abap_true.
      READ TABLE ms_data-socialsecurity WITH KEY personid = iv_struc-objid TRANSPORTING NO FIELDS .
      IF sy-subrc IS INITIAL.
        ms_common-hassocialbenefit = 1.
      ENDIF.
    ENDIF.
*} MAKAROVSV 19.09.2019

*    ms_common-hascertification       = get_common_hascertification( iv_struc = iv_struc ).
*    ms_common-hasadditionaleducation = get_common_hasadditionaleducat( iv_struc = iv_struc ).
*    ms_common-hassocialbenefit       = get_common_hassocialbenefit( iv_struc = iv_struc ).
*    ms_common-hasaward               = get_common_hasaward( iv_struc = iv_struc ).

*    ms_common-hasrelative = 0.
*    ms_common-hasrank = 0.
*
*    mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
*                                      i_begda         = ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
*                                      i_endda         = ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
*                                      i_infty         = `0081`
*                            IMPORTING e_pnnnn         = lt_p0081 ).
*    IF lt_p0081 IS NOT INITIAL.
*      ms_common-hasrank = 1.
*    ENDIF.
*    mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
*                                      i_begda         = ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
*                                      i_endda         = ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
*                                      i_infty         = `0021`
*                            IMPORTING e_pnnnn         = lt_p0021 ).
*    IF lt_p0021 IS NOT INITIAL.
*      ms_common-hasrelative = 1.
*    ENDIF.
*} KLOKOVNYU 17.06.2019
    COLLECT ms_common INTO ms_data-common.
  ENDMETHOD.
  METHOD fill_military.
    DATA: lt_p0290 TYPE TABLE OF p0290
        .
    CHECK ms_prop-per01 = abap_true.

    mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
                                          i_begda = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                          i_endda = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                          i_infty = `0290`
                                          i_subty = `07`
                                IMPORTING e_pnnnn = lt_p0290 ).
    LOOP AT lt_p0290 ASSIGNING FIELD-SYMBOL(<lt_p0290>).
      CLEAR: ms_military
           .
      ms_military-personid              = iv_struc-objid.  "Идентификатор сотрудника  табельный
*{ MAKAROVSV 08.11.2019
      "ms_military-militarystockcategory = <lt_p0290>-wdkat."Категория запаса
      ms_military-militarystockcategory = COND #(
        WHEN <lt_p0290>-wdkat = '1'
        THEN |Первая категория|
        WHEN <lt_p0290>-wdkat = '2'
        THEN |Вторая категория|
        ELSE ''
      ).                                                   "Категория запаса
*} MAKAROVSV 08.11.2019
      ms_military-militaryrank          = <lt_p0290>-wdstf."Воинское звание
      "ms_military-militarycomposition   = <lt_p0290>-wdstv."Состав (профиль)
      ms_military-militarycomposition   = 'командный'."Состав (профиль)
      ms_military-militaryvuskode       = <lt_p0290>-wdusp."Полное кодовое обозначение ВУС
*      ms_military-militaryreadycategory  = <lt_p0290>-wdpfl."Категория годности к военной службе
      IF t_cache_military IS INITIAL.
        SELECT *
          INTO TABLE @t_cache_military
          FROM t7rum0
            WHERE partn =  '01'.
      ENDIF.
      DATA ls_cache_military LIKE LINE OF t_cache_military.
      CLEAR ls_cache_military.
      READ TABLE t_cache_military INTO ls_cache_military
        WITH TABLE KEY primary_key COMPONENTS partn =  '01' cdpfl = <lt_p0290>-wdpfl.
      IF sy-subrc IS INITIAL.
        ms_military-militaryreadycategory  = |{ <lt_p0290>-wdpfl } - { ls_cache_military-wdbhd }|.
      ENDIF.

      ms_military-militarycommissariat  = <lt_p0290>-wdbhd."Наименование военного комиссариата по месту жительства
      COLLECT ms_military INTO  ms_data-military.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_baseedu.
    READ TABLE it_p0022 TRANSPORTING NO FIELDS WITH KEY zzbaseedu = 'X'.
    IF sy-subrc = 0.
      rv_ok = abap_true.
    ELSE.
      rv_ok = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD fill_education.
    DATA: lt_p0022 TYPE TABLE OF p0022
         ,lt_t7ruokso TYPE TABLE OF t7ruokso
         ,lv_subty TYPE subty
         ,lr_subty LIKE RANGE OF lv_subty
         ,lt_p0022_collect TYPE TABLE OF p0022
        .
    CHECK ms_prop-per02 = abap_true.

    lr_subty = VALUE #( ( sign = 'I' option = 'EQ' low = '03' )
                        ( sign = 'I' option = 'EQ' low = '07' )
                        ( sign = 'I' option = 'EQ' low = '10' )
                        ( sign = 'I' option = 'EQ' low = '11' )
                        ( sign = 'I' option = 'EQ' low = '15' )
                        ( sign = 'I' option = 'EQ' low = '18' ) ).
    LOOP AT lr_subty INTO DATA(ls_subty).
      mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
                                            i_begda = mv_cdate "ms_prop-begda[ 1 ]-high
                                            i_endda = mv_cdate "ms_prop-begda[ 1 ]-high
                                            i_infty = `0022`
                                            i_subty = ls_subty-low
                                  IMPORTING e_pnnnn = lt_p0022 ).
      APPEND LINES OF lt_p0022 TO lt_p0022_collect.
    ENDLOOP.
    lt_p0022 = lt_p0022_collect.

    DATA(lv_baseedu) = check_baseedu( lt_p0022 ).

    SORT: lt_p0022 BY subty DESCENDING begda DESCENDING
        .
    LOOP AT lt_p0022 ASSIGNING FIELD-SYMBOL(<lt_p0022>) WHERE NOT subty IN ms_prop-sub22.
      IF lv_baseedu IS INITIAL.
        DATA(lv_flagmain) = sy-tabix.
      ENDIF.
      CLEAR: ms_education.

      "Идентификатор
      ms_education-educationbasicid      = cacheid(
          i_personid    = CONV #( iv_struc-objid )
          i_block       = '02'
          i_block_type  = <lt_p0022>-subty
          i_block_date  = <lt_p0022>-begda
          i_last_date   = ms_prop-begda[ 1 ]-high ).

      "ИдентийикаторИдентификатор сотрудника
      ms_education-personid = iv_struc-objid .

      "ИдентийикаторНаименование образовательного учреждения
      IF <lt_p0022>-wide_name IS NOT INITIAL.
        ms_education-institutiontext = <lt_p0022>-wide_name.
      ELSE.
        ms_education-institutiontext = get_name_insti( iv_insti = <lt_p0022>-insti ).
      ENDIF.
      IF ms_education-institutiontext IS INITIAL.
        ms_education-institutiontext = <lt_p0022>-insti.
      ENDIF.

      "ИдентийикаторГод начала
      IF <lt_p0022>-zstart IS  NOT INITIAL.
        ms_education-datestart = |{ <lt_p0022>-zstart DATE = USER }|.
      ENDIF.
      IF <lt_p0022>-zend IS INITIAL.
        ms_education-enddate = |{ <lt_p0022>-zzdatdc DATE = USER }|.
      ELSE.
        ms_education-enddate = |{ <lt_p0022>-zend DATE = USER }|.
      ENDIF.

      "ИдентийикаторНаименование документа об образовании
      ms_education-namedocumenteducation = <lt_p0022>-slabs.

      SELECT SINGLE stext
        INTO ms_education-namedocumenteducation
        FROM t519t
        WHERE slabs = <lt_p0022>-slabs
          AND sprsl = 'RU'.

      ms_education-withhonorsdiploma = '0'.
      IF <lt_p0022>-zzreddp IS NOT INITIAL.
        ms_education-withhonorsdiploma = '1'.
      ENDIF.

      ms_education-qualification = <lt_p0022>-zzeduqlf.

      IF <lt_p0022>-zzdatdc IS NOT INITIAL.
        ms_education-educationdocdate = |{ <lt_p0022>-zzdatdc DATE = USER }|.
      ENDIF.
      IF <lt_p0022>-zzshort IS NOT INITIAL.
        DATA lt_cocso TYPE RANGE OF t7ruokso-cokso.
        CLEAR lt_cocso.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <lt_p0022>-zzshort ) TO lt_cocso.
        DATA lv_cocso TYPE t7ruokso-cokso.
        lv_cocso = <lt_p0022>-zzshort.
        DO strlen( <lt_p0022>-zzshort ) TIMES.
          IF lv_cocso(1) = '0'.
            lv_cocso = lv_cocso+1.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_cocso ) TO lt_cocso.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

        DATA lv_data TYPE d.
        lv_data = mv_cdate. "ms_prop-begda[ 1 ]-high.
        SELECT *
          FROM t7ruokso
          INTO TABLE lt_t7ruokso
          WHERE cokso IN lt_cocso
            AND endda >= lv_data
            AND begda <= lv_data
        .
        SORT lt_t7ruokso BY cokso.
        READ TABLE lt_t7ruokso INTO DATA(ls_t7ruokso) INDEX 1.
        "ms_education-qualification = ls_t7ruokso-cname.
        ms_education-speciality = ls_t7ruokso-cname.
      ENDIF.

      "ИдентийикаторСерия диплома + ИдентийикаторНомер диплома
      IF <lt_p0022>-zzseria IS NOT INITIAL.
        ms_education-educationdocseries = <lt_p0022>-zzseria.
        ms_education-educationdocnumber = <lt_p0022>-ksbez.
      ELSE.
        IF <lt_p0022>-ksbez IS NOT INITIAL.
          SPLIT <lt_p0022>-ksbez AT space INTO TABLE DATA(lt_rows).
          DESCRIBE TABLE lt_rows LINES DATA(lv_rows_namber).
          IF lv_rows_namber = 1.
            ms_education-educationdocseries = '0'.
            ms_education-educationdocnumber = <lt_p0022>-ksbez.
          ELSE.
            ms_education-educationdocseries = lt_rows[ 1 ].
            ms_education-educationdocnumber = lt_rows[ lv_rows_namber ].

          ENDIF.

        ELSE.
          ms_education-educationdocseries = '0'.
          ms_education-educationdocnumber = '0'.
        ENDIF.
      ENDIF.
      REPLACE '№' IN ms_education-educationdocnumber WITH ''.
      CONDENSE:
          ms_education-educationdocseries
        , ms_education-educationdocnumber
        .
      "Признак приоритетного образования
      IF lv_baseedu = abap_false.
        IF lv_flagmain = 1.
          ms_education-flagmain = '1'.
        ELSE.
          ms_education-flagmain = '0'.
        ENDIF.
      ELSE.
        IF <lt_p0022>-zzbaseedu = 'X'.
          ms_education-flagmain = '1'.
        ELSE.
          ms_education-flagmain = '0'.
        ENDIF.
      ENDIF.

      ms_education-specialityokso	       = <lt_p0022>-zzshort.        "ИдентийикаторНаправление или специальность по документу код по ОКСО

      IF <lt_p0022>-zzfrofs IN t_cache_education_3x.
        "Для старого справочника
        CASE <lt_p0022>-subty.
          WHEN '11'.
            ms_education-educationokin = |05|.
            ms_education-education = |Среднее профессиональное образование|.

          WHEN '18'.
            IF ms_education-qualification CS 'Бакалавр' OR
               ms_education-qualification CS 'бакалавр' OR
               ms_education-speciality    CS 'Бакалавр' OR
               ms_education-speciality    CS 'бакалавр'.
              ms_education-educationokin = |06|.
              ms_education-education = |Высшее образование – бакалавриат|.
            ELSEIF
              ms_education-qualification CS 'Магистр' OR
              ms_education-qualification CS 'магистр' OR
              ms_education-speciality    CS 'Магистр' OR
              ms_education-speciality    CS 'магистр'.
              ms_education-educationokin = |07|.
              ms_education-education = |Высшее образование – магистратура|.
            ELSE.
              ms_education-educationokin = |07|.
              ms_education-education = |Высшее образование – специалитет|.
            ENDIF.

          WHEN OTHERS.
            IF <lt_p0022>-subty = '07' OR
               <lt_p0022>-subty = '10' .
              ms_education-educationokin = |04|.
            ELSE.
              ms_education-educationokin = <lt_p0022>-slart.
            ENDIF.
            SELECT SINGLE cname
              INTO ms_education-education
              FROM t7ruokin
              WHERE facet = '30'
                AND ccode = <lt_p0022>-subty.
        ENDCASE.
      ELSE.
        "Для нового справочника
        CASE <lt_p0022>-zzshort+4(2).
          WHEN '01' OR '02'.
            ms_education-educationokin = |05|.
            ms_education-education = |Среднее профессиональное образование|.
          WHEN '03'.
            ms_education-educationokin = |06|.
            ms_education-education = |Высшее образование – бакалавриат|.
          WHEN '04'.
            ms_education-educationokin = |07|.
            ms_education-education = |Высшее образование – магистратура|.
          WHEN '05'.
            ms_education-educationokin = |07|.
            ms_education-education = |Высшее образование – специалитет|.
          WHEN OTHERS.
            ms_education-educationokin = |08|.
            ms_education-education = |Высшее образование - подготовка кадров высшей квалификации|.
        ENDCASE.
      ENDIF.

      COLLECT ms_education INTO  ms_data-education.
    ENDLOOP.
  ENDMETHOD.
  METHOD fill_orderappointment.
    CHECK ms_prop-per03 = abap_true.

    DATA: lt_p0001 TYPE TABLE OF p0001
        , lt_p0294 TYPE TABLE OF p0294
        , lt_p0016 TYPE TABLE OF p0016
        .
    CLEAR ms_orderappointment.

    mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
                                          i_endda = mv_cdate "ms_prop-begda[ 1 ]-high
                                          i_infty = `0001`
                                IMPORTING e_pnnnn = lt_p0001 ).

    CHECK lt_p0001 IS NOT INITIAL.
    SORT lt_p0001 BY begda DESCENDING.

    ms_orderappointment-orderappointmentid = ``."Уникальный идентификатор
    ms_orderappointment-personid           = iv_struc-objid."Идентификатор сотрудника

    READ TABLE lt_p0001 ASSIGNING FIELD-SYMBOL(<lt_p0001>) INDEX 1.
    IF sy-subrc = 0 AND <lt_p0001> IS ASSIGNED.
      ms_orderappointment-organizationid     = <lt_p0001>-orgeh."Идентификатор подразделения
      ms_orderappointment-positionid         = <lt_p0001>-stell."Идентификатор должности
*{ 01/06/2020
      LOOP AT lt_p0001 ASSIGNING FIELD-SYMBOL(<zt_p0001>) WHERE
*                                                             orgeh     = <lt_p0001>-orgeh
*                                                            AND stell     = <lt_p0001>-stell
                                                              plans = <lt_p0001>-plans
                                                             AND NOT plans = `99999999`.
*} 01/06/2020
        IF <lt_p0001>-endda IS NOT INITIAL.
          IF ms_orderappointment-dateend IS INITIAL.
            ms_orderappointment-dateend = |{ <lt_p0001>-endda DATE = USER }|.    "Дата окончания
          ENDIF.
        ENDIF.
        IF ( <lt_p0001>-werks = '0160' OR
             <lt_p0001>-werks = '0161' OR
             <lt_p0001>-werks = '0162' OR
             <lt_p0001>-werks = '0163' OR
             <lt_p0001>-werks = '0164' OR
             <lt_p0001>-werks = '0165' ) AND <lt_p0001>-begda <= '20190101'.
          CLEAR ms_orderappointment-datestart .       "Дату назначения получим ниже
        ELSE.
*{ 01/06/2020
          IF <zt_p0001>-begda IS NOT INITIAL.

            ms_orderappointment-datestart = |{ <zt_p0001>-begda DATE = USER }|.  "Дата назначения
          ENDIF.
*} 01/06/2020
        ENDIF.
      ENDLOOP.
    ENDIF.

    mo_get_data->read_pa_infty(
      EXPORTING
        i_pernr = CONV #( iv_struc-objid )
        i_endda = mv_cdate "ms_prop-begda[ 1 ]-high
        i_begda = mv_cdate "ms_prop-begda[ 1 ]-high
        i_infty = `0294`
      IMPORTING
        e_pnnnn = lt_p0294 ).

    DATA lt_p0298 TYPE TABLE OF p0298.
    mo_get_data->read_pa_infty(
      EXPORTING
        i_pernr = CONV #( iv_struc-objid )
        i_endda = mv_cdate "ms_prop-begda[ 1 ]-high
        i_infty = `0298`
      IMPORTING
        e_pnnnn = lt_p0298 ).

    DATA lt_massn TYPE RANGE OF pa0298-massn.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'X1' ) TO lt_massn.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'X2' ) TO lt_massn.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'X3' ) TO lt_massn.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'X4' ) TO lt_massn.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'Z8' ) TO lt_massn.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'Z9' ) TO lt_massn.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'T1' ) TO lt_massn.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'T3' ) TO lt_massn.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'W6' ) TO lt_massn.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'W7' ) TO lt_massn.
    DELETE lt_p0298 WHERE massn NOT IN  lt_massn.

    LOOP AT lt_p0294 ASSIGNING FIELD-SYMBOL(<lt_p0294>).
      IF ms_orderappointment-datestart IS INITIAL .
        ms_orderappointment-datestart = |{ <lt_p0294>-begda DATE = USER }|.             "Дата назначения
      ENDIF.
      READ TABLE lt_p0298 INTO DATA(ls_p0298) WITH KEY begda = <lt_p0294>-begda subty = space.
      IF sy-subrc IS INITIAL.
        ms_orderappointment-ordernumb = ls_p0298-ordnu.                                 "Номер приказа
        IF ls_p0298-orddt IS NOT INITIAL.
          ms_orderappointment-orderdate = |{ ls_p0298-orddt DATE = USER }|.             "Дата приказа
        ENDIF.
      ENDIF.

      IF ms_orderappointment-ordernumb IS INITIAL.
        ms_orderappointment-ordernumb  = <lt_p0294>-ordnu.                              "Номер приказа
      ENDIF.
      IF ms_orderappointment-orderdate IS INITIAL.
        IF <lt_p0294>-orddt IS NOT INITIAL.
          ms_orderappointment-orderdate = |{ <lt_p0294>-orddt DATE = USER }|.           "Дата приказа
        ENDIF.
      ENDIF.

      ms_orderappointment-orderappointmentid = cacheid(                               "Идентификатор
          i_personid    = CONV #( iv_struc-objid )
          i_block       = '04'
          i_block_type  = ''
          i_block_date  = <lt_p0294>-begda
          i_last_date   = ms_prop-begda[ 1 ]-high
        ).
    ENDLOOP.
    IF ms_orderappointment-datestart = '@' .
      "Записей в 0294 ИТ не существует
      CLEAR ms_orderappointment-datestart.
    ENDIF.
    mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
                                          i_endda = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                          i_begda = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                          i_infty = `0016`
                                IMPORTING e_pnnnn = lt_p0016 ).
    "Характер работы
    LOOP AT lt_p0016 ASSIGNING FIELD-SYMBOL(<lt_p0016>).
      CASE <lt_p0016>-cttyp.
        WHEN `01` OR `03`.
          ms_orderappointment-orderperiodtype    = `1`.
        WHEN `02` OR `04`.
          ms_orderappointment-orderperiodtype    = `0`.
        WHEN OTHERS.
          "
      ENDCASE.

      IF <lt_p0016>-ctedt IS NOT INITIAL.
        ms_orderappointment-dateend = |{ <lt_p0016>-ctedt DATE = USER }|.
      ENDIF.
    ENDLOOP.

    IF ms_orderappointment-orderperiodtype IS INITIAL .
      ms_orderappointment-orderperiodtype    = `0`.
    ENDIF.

    mo_get_data->get_objid_struc( EXPORTING i_otype = iv_struc-otype
                                          i_objid = CONV #( iv_struc-objid )
                                          i_wegid = 'P-S-O'
                                          i_begda = mv_cdate "ms_prop-begda[ 1 ]-high
                                          i_vflag = abap_true
                                IMPORTING e_struc = DATA(lt_struc) ).
    READ TABLE lt_struc WITH KEY otype = 'S' INTO DATA(ls_struc).
    ms_orderappointment-staffunitid = ls_struc-objid.

    ms_orderappointment-flagmain = `0`.                     " Вид работы

    ms_orderappointment-substitutepersonid = get_substitutepersonid(  " Идентификатор замещенного сотрудника
      EXPORTING i_begda = mv_cdate "ms_prop-begda[ 1 ]-low
                i_endda = mv_cdate "ms_prop-begda[ 1 ]-high
                i_objid = CONV #( iv_struc-objid )
      IMPORTING e_prozt = ms_orderappointment-sherate ).

    COLLECT ms_orderappointment INTO  ms_data-orderappointment.
  ENDMETHOD.
  METHOD fill_workactivitie.
    DATA: lt_p0294 TYPE TABLE OF p0294
        , lv_datum TYPE text10
        .

    CHECK ms_prop-per04 = abap_true.

    mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
                                          i_endda = mv_cdate "ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
                                          i_infty = `0294`
                                IMPORTING e_pnnnn = lt_p0294 ).
    SORT: lt_p0294 BY begda ASCENDING. " DESCENDING. MAKAROVSV 16.09.2019

    LOOP AT lt_p0294 ASSIGNING FIELD-SYMBOL(<lt_p0294>).
      CLEAR: ms_workactivitie
           .
      ms_workactivitie-addressorganization = COND #(                                       "Адрес организации
        WHEN <lt_p0294>-zzaddresorganization IS INITIAL
        THEN 'г. Санкт-Петербург'
        ELSE <lt_p0294>-zzaddresorganization ).
      ms_workactivitie-personid            = iv_struc-objid.                               "Идентификатор сотрудника
      ms_workactivitie-organizationtext    = <lt_p0294>-arbgb && `, ` && <lt_p0294>-deprt. "Наименование организации и подразделения                                     "Адрес организации
      ms_workactivitie-positiontext        = <lt_p0294>-posit.                             "Наименование должности
      WRITE <lt_p0294>-begda TO lv_datum LEFT-JUSTIFIED.
      ms_workactivitie-datestart           = lv_datum.                                     "Дата начала
      WRITE <lt_p0294>-endda TO lv_datum LEFT-JUSTIFIED.
      ms_workactivitie-dateend             = lv_datum.                                     "Дата окончания

*{ MAKAROVSV 16.09.2019
      IF <lt_p0294>-begda <= ms_prop-begda[ 1 ]-high AND
        ms_prop-begda[ 1 ]-high <= <lt_p0294>-endda.
        CLEAR ms_workactivitie.
        EXIT.
      ENDIF.
      ms_workactivitie-workactivitieid     = cacheid(                                      "Уникальный идентификатор
        i_block      = '05'
        i_block_date = <lt_p0294>-begda
        i_block_type = ''
        i_personid   = CONV #( iv_struc-objid )
        i_last_date  = ms_prop-begda[ 1 ]-high ).
*} MAKAROVSV 16.09.2019

      "Относится к общему стажу (признак)
      ms_workactivitie-experiencegeneral = `0`.
      IF <lt_p0294>-tls01 = abap_true.
        ms_workactivitie-experiencegeneral = `1`.
      ENDIF.
      "Относится к стажу государственной службы (признак)
      ms_workactivitie-experiencestate = `0`.
      IF <lt_p0294>-tls03 = abap_true OR
         <lt_p0294>-tls04 = abap_true .
        ms_workactivitie-experiencestate = `1`.
      ENDIF.
      COLLECT ms_workactivitie INTO  ms_data-workactivitie.
    ENDLOOP.
  ENDMETHOD.
*{ MAKAROVSV 20.09.2019
  METHOD fill_foreignlanguage.
    DATA:
       lt_obj  TYPE TABLE OF hrsobid
     , lt_profile TYPE TABLE OF hrpe_profq
     .
    CHECK ms_prop-per12 = abap_true.

    CLEAR lt_obj.
    APPEND VALUE #( plvar = '01' otype = 'P' sobid = iv_struc-objid ) TO lt_obj.
    CALL FUNCTION 'RHPP_Q_PROFILE_READ'
      EXPORTING
        begda            = mv_cdate "ms_prop-begda[ 1 ]-high
        endda            = mv_cdate "ms_prop-begda[ 1 ]-high
      TABLES
        objects          = lt_obj
        profile          = lt_profile
      EXCEPTIONS
        no_authority     = 1
        wrong_otype      = 2
        object_not_found = 3
        undefined        = 4
        OTHERS           = 5.

    IF v_cache_foreignlanguage IS INITIAL.
      SELECT SINGLE
        low
        INTO v_cache_foreignlanguage
        FROM tvarvc
        WHERE name = 'ZZ_HR_LANGUAGE' AND
              type = 'P'.
    ENDIF.
    DELETE lt_profile WHERE class_id NE v_cache_foreignlanguage.

    LOOP AT lt_profile ASSIGNING FIELD-SYMBOL(<fs_profile>).
      CLEAR: ms_foreignlanguage.

      DATA(ls_t7ruokin) = VALUE #( t_cache_foreignlanguage[ KEY primary_key COMPONENTS cname = <fs_profile>-ttext ] OPTIONAL ).
      IF ls_t7ruokin IS INITIAL.
        SELECT SINGLE
            *
          INTO @ls_t7ruokin
          FROM t7ruokin
          WHERE molga = '33' AND
                sprsl = 'RU' AND
                facet = '04' AND
                cname = @<fs_profile>-ttext.

        IF ls_t7ruokin IS INITIAL.
          DATA(lv_error_message) = |{ text-008 } { iv_struc-objid }.|.
          MESSAGE lv_error_message TYPE 'E' .
        ENDIF.

        INSERT ls_t7ruokin INTO TABLE t_cache_foreignlanguage.
      ENDIF.

      ms_foreignlanguage-personid           = iv_struc-objid.
      ms_foreignlanguage-languageokin       = ls_t7ruokin-cname.
      ms_foreignlanguage-languagedegreeokin = <fs_profile>-profc_text.
      ms_foreignlanguage-foreignlanguageid  = cacheid(
          i_personid    = CONV #( iv_struc-objid )
          i_block       = '01'
          i_block_type  = ls_t7ruokin-ccode
          i_block_date  = <fs_profile>-vbegd
          i_last_date   = ms_prop-begda[ 1 ]-high ).

      COLLECT ms_foreignlanguage INTO ms_data-foreignlanguage.
      CLEAR ls_t7ruokin.
    ENDLOOP.
  ENDMETHOD.
*} MAKAROVSV 20.09.2019
  METHOD is_redun.
    DATA: lt_p1014 TYPE TABLE OF p1014
        .

* -- Должность
    CALL FUNCTION 'RH_READ_INFTY'
      EXPORTING
        authority            = ' '
        with_stru_auth       = ' '
        plvar                = '01'
        otype                = 'S'
        objid                = i_objid
        infty                = '1014'
        begda                = i_begda
        endda                = i_endda
      TABLES
        innnn                = lt_p1014
      EXCEPTIONS
        all_infty_with_subty = 1
        nothing_found        = 2
        no_objects           = 3
        wrong_condition      = 4
        wrong_parameters     = 5
        OTHERS               = 6.
    IF sy-subrc = 0.
      READ TABLE lt_p1014 INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p0014>).
      IF sy-subrc EQ 0.
        r_yes     = <fs_p0014>-redun.
        c_dateend = |{ <fs_p0014>-begda DATE = USER }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD is_vacancy.
    DATA: lt_p1007 TYPE TABLE OF p1007
        .

    CALL FUNCTION 'RH_READ_INFTY'
      EXPORTING
        plvar                = '01'
        otype                = 'S'
        objid                = i_objid
        infty                = '1007'
        begda                = i_begda
        endda                = i_endda
      TABLES
        innnn                = lt_p1007
      EXCEPTIONS
        all_infty_with_subty = 1
        nothing_found        = 2
        no_objects           = 3
        wrong_condition      = 4
        wrong_parameters     = 5
        OTHERS               = 6.

    READ TABLE lt_p1007 INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p1007>).
    IF sy-subrc EQ 0 AND
        <fs_p1007>-status = '0'.
      r_yes = abap_true.
    ENDIF.
  ENDMETHOD.
  METHOD fill_header.
    DATA: lv_p TYPE p LENGTH 16 DECIMALS 1
        .

    ms_data-header-organizationuid = p_uid.
    ms_data-header-informationsystemname = text-001.
    SELECT SINGLE release FROM cvers
      INTO @DATA(lv_release)
      WHERE component EQ 'SAP_ABA'.
    IF sy-subrc EQ 0.
      lv_p = lv_release / 100.
      ms_data-header-informationsystemversion = lv_p.
      CONDENSE ms_data-header-informationsystemversion NO-GAPS.
    ENDIF.

    ms_data-header-importtypeid = '0'.
    ms_data-header-datecreation = |{ sy-datum DATE = USER }|.

    ms_data-header-importtypeid = ms_prop-itype. CONDENSE  ms_data-header-importtypeid.

    DESCRIBE TABLE mt_plans_count LINES ms_data-header-limitnumbers.
  ENDMETHOD.
  METHOD check_mandatory_fld.
    IF cv_fld IS INITIAL.
      cv_fld = 'значение не найдено'(004).
    ENDIF.
  ENDMETHOD.
  METHOD format_pernr.
*    SHIFT cv_fld LEFT DELETING LEADING '0'." KlokovNYU 03.07.2019 Оставляем нули на месте
  ENDMETHOD.
  METHOD get_upper_orgeh.
    DATA: lt_p1001 TYPE TABLE OF p1001
        .

    mo_get_data->read_om_infty(
      EXPORTING
        i_otype     = 'O'    " Тип объекта
        i_objid     = i_objid    " Идентификатор объекта
        i_infty     = '1001'     " Инфо-тип
        i_subty     = 'A002'
        i_authority = space    " Общий флаг
        i_sauth     = space    " Общий флаг
        i_begda     = ms_prop-begda[ 1 ]-high    " Начало срока действия "MAKAROVSV 16.09.2019
        i_endda     = ms_prop-begda[ 1 ]-high    " Конец срока действия  "MAKAROVSV 16.09.2019
      IMPORTING
        e_pnnnn     = lt_p1001 ).
    READ TABLE lt_p1001 INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p1001>).
    IF sy-subrc EQ 0.
      r_obj = <fs_p1001>-sobid.
    ENDIF.
  ENDMETHOD.
  METHOD fill_ordersalary.
    DATA: lt_p0014      TYPE TABLE OF p0014
        , lt_p1005      TYPE TABLE OF p1005
        , lt_p1005_date TYPE TABLE OF p1005.

    DATA: lv_begda TYPE begda
          , lv_endda TYPE begda.

    CHECK ms_prop-per05 = abap_true.
    CLEAR ms_ordersalary.

    mo_get_data->read_om_infty(
      EXPORTING
        i_otype     = 'C'                         " Тип объекта
        i_objid     = ms_p0001-stell              " Идентификатор объекта
        i_infty     = '1005'                      " Инфо-тип
        i_authority = space                       " Общий флаг
        i_sauth     = space                       " Общий флаг
        i_begda     = mv_cdate "ms_prop-begda[ 1 ]-high     " Начало срока действия
        i_endda     = mv_cdate "ms_prop-begda[ 1 ]-high     " Конец срока действия
      IMPORTING
        e_pnnnn     = lt_p1005
    ).
    READ TABLE lt_p1005 INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_p1005>).

    mo_get_data->read_pa_infty(
      EXPORTING
        i_pernr = CONV #( iv_struc-objid )
        i_begda = mv_cdate "ms_prop-begda[ 1 ]-high
        i_endda = mv_cdate "ms_prop-begda[ 1 ]-high
        i_infty = `0014`
      IMPORTING
        e_pnnnn = lt_p0014
    ).
    LOOP AT lt_p0014 ASSIGNING FIELD-SYMBOL(<fs_p0014>).
      CLEAR ms_ordersalary.
      ms_ordersalary-personid = iv_struc-objid.
      IF <fs_p1005> IS ASSIGNED.
        ms_ordersalary-positionsalaryvalue = <fs_p1005>-cpmin.
      ENDIF.
*{ MAKAROVSV 23.09.2019
      CASE <fs_p0014>-lgart.
        WHEN '1010'.
          ms_ordersalary-salarytypeid = |за классный чин|.
        WHEN '1020'.
          ms_ordersalary-salarytypeid = |за особые условия гражданской службы|.
        WHEN '1030'.
          ms_ordersalary-salarytypeid = |за стаж государственной гражданской службы (выслугу лет)|.
        WHEN '1040'.
          ms_ordersalary-salarytypeid = |ежемесячное денежное поощрение|.
        WHEN '1050'.
          ms_ordersalary-salarytypeid = |процентная надбавка к должностному окладу за стаж работы в структурных подразделениях по защите государственной тайны|.
        WHEN '1060'.
          ms_ordersalary-salarytypeid = |ежемесячная надбавка за сложность, напряженность и высокие достижения в труде|.
        WHEN OTHERS.
          ms_ordersalary-salarytypeid = <fs_p0014>-lgart.
      ENDCASE.

      IF <fs_p0014>-anzhl IS NOT INITIAL.
        DATA lv_variable(16) TYPE p DECIMALS 2.
        IF <fs_p1005> IS ASSIGNED.
          lv_variable = ( <fs_p0014>-anzhl / 100 ) * <fs_p1005>-cpmin.
        ENDIF.
        "       ms_ordersalary-salaryvalue = |{ lv_variable }|.
        ms_ordersalary-salaryvalue = <fs_p0014>-anzhl.
        IF <fs_p1005> IS ASSIGNED.
          ms_ordersalary-salaryvalueruble = <fs_p1005>-cpmin.
        ENDIF.
      ELSE.
        ms_ordersalary-salaryvalue = <fs_p0014>-betrg.
        ms_ordersalary-salaryvalueruble = <fs_p0014>-betrg.
      ENDIF.

      "      ms_ordersalary-salaryvalueruble = ms_ordersalary-salaryvalue.

      ms_ordersalary-ordersalaryid    = cacheid(            "Идентификатор
        i_block      = '06'
        i_block_date = <fs_p0014>-begda
        i_block_type = <fs_p0014>-subty
        i_personid   = CONV #( iv_struc-objid )
        i_last_date  = ms_prop-begda[ 1 ]-high
      ).
      ms_ordersalary-orderrankid      = COND #(             "Ссылка на запись из объекта «Классный чин, воинское звание»
        WHEN <fs_p0014>-lgart = '1010'
        THEN ms_orderranks-orderrankid
        ELSE ''
      ).
      ms_ordersalary-ordernumb          = '1'.              "Номер приказа
*      IF ms_ordersalary-salarytypeid IS NOT INITIAL.
*       READ TABLE lt_p0014 ASSIGNING FIELD-SYMBOL(<wa_p0014>) WITH KEY lgart = <fs_p0014>-lgart .
*       IF sy-subrc IS INITIAL.
*      ms_ordersalary-datestart = |{ <wa_p0014>-begda DATE = USER }|.
*      IF <wa_p0014>-endda < '99991231'.
*        ms_ordersalary-dateend = |{ <wa_p0014>-endda DATE = USER }|.
*      ENDIF.
*       ENDIF.
      ms_ordersalary-datestart = |{ <fs_p0014>-begda DATE = USER }|.
      IF <fs_p0014>-endda < '99991231'.
        ms_ordersalary-dateend = |{ <fs_p0014>-endda DATE = USER }|.
      ENDIF.
*      ELSE.
*        IF <fs_p1005> IS ASSIGNED.
*          ms_ordersalary-datestart = |{ <fs_p1005>-begda DATE = USER }|.
*          IF <fs_p1005>-endda < '99991231' .
*            ms_ordersalary-dateend = |{ <fs_p1005>-endda DATE = USER }|.
*          ENDIF.
*        ENDIF.
*      ENDIF.
      ms_ordersalary-orderdate = ms_ordersalary-datestart.

      COLLECT ms_ordersalary INTO ms_data-ordersalary.
    ENDLOOP.
*    IF sy-subrc NE 0.
    DATA: lt_p1001 TYPE TABLE OF p1001 .
    mo_get_data->read_om_infty(
      EXPORTING
        i_plvar     = '01'
        i_otype     = 'P'
        i_objid     = CONV #( iv_struc-objid )
        i_infty     = '1001'
        i_authority = space
        i_sauth     = space
        i_begda     = mv_cdate "ms_prop-begda[ 1 ]-high
        i_endda     = mv_cdate "ms_prop-begda[ 1 ]-high
      IMPORTING
        e_pnnnn     = lt_p1001 ).
    DELETE lt_p1001 WHERE sclas <> 'S'.
    IF lt_p1001 IS NOT INITIAL.
      DATA(wa_p1001) = lt_p1001[ 1 ].
      CLEAR lt_p1001.
      mo_get_data->read_om_infty(
        EXPORTING
          i_plvar     = '01'
          i_otype     = 'S'
          i_objid     = CONV #( wa_p1001-sobid )
          i_infty     = '1001'
          i_authority = space
          i_sauth     = space
          i_begda     = mv_cdate "ms_prop-begda[ 1 ]-high
          i_endda     = mv_cdate "ms_prop-begda[ 1 ]-high
        IMPORTING
          e_pnnnn     = lt_p1001
      ).
      wa_p1001 = VALUE #( lt_p1001[ sobid = iv_struc-objid  sclas = 'P' ] OPTIONAL ).
      IF sy-subrc IS INITIAL.
        ms_ordersalary-datestart          = |{ wa_p1001-begda DATE = USER }|.
        lv_begda = wa_p1001-begda.
        IF wa_p1001-endda <> '99991231'.
          ms_ordersalary-dateend          = |{ wa_p1001-endda DATE = USER }|.
          lv_endda = wa_p1001-endda.
        ENDIF.
*          IF ms_ordersalary-orderdate IS INITIAL.
        ms_ordersalary-orderdate =  ms_ordersalary-datestart.
*          ENDIF.
      ENDIF.
    ENDIF.
*    ENDIF.
    IF <fs_p1005> IS ASSIGNED.
      ms_ordersalary-ordersalaryid = cacheid(            "Идентификатор
        i_block      = '06'
        i_block_date = <fs_p1005>-begda
        i_block_type = ''
        i_personid   = CONV #( iv_struc-objid )
        i_last_date  = mv_cdate ) ."ms_prop-begda[ 1 ]-high ).

      ms_ordersalary-personid = iv_struc-objid.
      ms_ordersalary-salarytypeid = 'должностной оклад'.
      ms_ordersalary-positionsalaryvalue = <fs_p1005>-cpmin.
      ms_ordersalary-salaryvalueruble = <fs_p1005>-cpmin.
      ms_ordersalary-salaryvalue = <fs_p1005>-cpmin..
      ms_ordersalary-ordernumb = '1'.

      IF lv_begda < <fs_p1005>-begda.
        ms_ordersalary-datestart = |{ <fs_p1005>-begda DATE = USER }|.
        ms_ordersalary-orderdate =  ms_ordersalary-datestart.
      ENDIF.
      IF <fs_p1005>-endda < '99991231' .
        IF ms_ordersalary-dateend IS INITIAL.
          ms_ordersalary-dateend = |{ <fs_p1005>-endda DATE = USER }|.
        ELSE.
          IF lv_endda > <fs_p1005>-endda.
            ms_ordersalary-dateend = |{ <fs_p1005>-endda DATE = USER }|.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ms_ordersalary-salaryvalueruble IS INITIAL.
        ms_ordersalary-salaryvalueruble = ms_ordersalary-positionsalaryvalue * ms_ordersalary-salaryvalue.
      ENDIF.

      COLLECT ms_ordersalary INTO ms_data-ordersalary.
    ENDIF.
  ENDMETHOD.
  METHOD fill_relatives.
    DATA lt_p0021 TYPE TABLE OF p0021 .

    CHECK ms_prop-per06 = abap_true.
    CLEAR ms_relatives.

    ms_relatives-personid = iv_struc-objid.
    mo_get_data->read_pa_infty(
      EXPORTING
        i_pernr = CONV #( iv_struc-objid )
        i_begda = mv_cdate "ms_prop-begda[ 1 ]-high
        i_endda = mv_cdate "ms_prop-begda[ 1 ]-high
        i_infty = `0021`
      IMPORTING
        e_pnnnn = lt_p0021 ).
    LOOP AT lt_p0021 ASSIGNING FIELD-SYMBOL(<fs_p0021>).
*{ MAKAROVSV 23.09.2019
      ms_relatives-relativeid  = cacheid(
        i_block      = '07'
        i_block_date = <fs_p0021>-fgbdt
        i_block_type = |{ <fs_p0021>-subty(2) }{ <fs_p0021>-objps(2) }|
        i_last_date  = mv_cdate "ms_prop-begda[ 1 ]-high
        i_personid   = CONV #( iv_struc-objid )
      ).
      ms_relatives-kinshipokin = VALUE #( t_cache_relatives_kinshipokin[
          KEY primary_key COMPONENTS ccode = <fs_p0021>-famsa ]-cname OPTIONAL ).
      IF ms_relatives-kinshipokin IS INITIAL.
        SELECT SINGLE
          *
          INTO @DATA(ls_t7ruokin)
          FROM t7ruokin
          WHERE molga = '33' AND
                sprsl = 'R' AND
                facet = '11' AND
                ccode = @<fs_p0021>-famsa.
        IF ls_t7ruokin IS NOT INITIAL.
          INSERT ls_t7ruokin INTO TABLE t_cache_relatives_kinshipokin.
          ms_relatives-kinshipokin = ls_t7ruokin-cname.
          CLEAR ls_t7ruokin.
        ENDIF.
      ENDIF.
*} MAKAROVSV 23.09.2019

      ms_relatives-lastname    = <fs_p0021>-fanam.
      ms_relatives-firstname   = <fs_p0021>-favor.
      ms_relatives-middlename  = <fs_p0021>-fnac2.
      ms_relatives-dateofbirth = |{ <fs_p0021>-fgbdt DATE = USER }|.
      APPEND ms_relatives TO ms_data-relatives.
    ENDLOOP.
  ENDMETHOD.
  METHOD fill_educationadditionals.
    DATA lt_p0022 TYPE TABLE OF p0022.

    CHECK ms_prop-per07 = abap_true.
    CLEAR ms_educationadditional.

    mo_get_data->read_pa_infty(
      EXPORTING
        i_pernr = CONV #( iv_struc-objid )
        i_begda = mv_cdate "ms_prop-begda[ 1 ]-high
        i_endda = mv_cdate "ms_prop-begda[ 1 ]-high
        i_infty = `0022`
      IMPORTING
        e_pnnnn = lt_p0022 ).
    DELETE lt_p0022 WHERE subty NE '91' AND subty NE '92'.
    ms_educationadditional-personid = iv_struc-objid.
    LOOP AT lt_p0022 ASSIGNING FIELD-SYMBOL(<fs_p0022>).
      ms_educationadditional-educationadditionalid = cacheid(                 "Идентификатор
        i_block       = '09'
        i_block_date  = <fs_p0022>-begda
        i_block_type  = <fs_p0022>-subty
        i_last_date   = ms_prop-begda[ 1 ]-high
        i_personid    = CONV #( iv_struc-objid ) ).
      IF <fs_p0022>-zend IS NOT INITIAL.
        ms_educationadditional-enddate = |{ <fs_p0022>-zend DATE = USER }|.     "Дата окончания
      ENDIF.
      IF <fs_p0022>-zzseria IS INITIAL.
        ms_educationadditional-educationdocnumber = <fs_p0022>-ksbez.
      ELSE.
        ms_educationadditional-educationdocnumber = |{ <fs_p0022>-zzseria } { <fs_p0022>-ksbez }|.
        CONDENSE ms_educationadditional-educationdocnumber.
      ENDIF.
      IF ms_educationadditional-educationdocnumber IS INITIAL.
        ms_educationadditional-educationdocnumber = '0'.
      ENDIF.
      IF <fs_p0022>-zstart IS NOT INITIAL.
        ms_educationadditional-startdate = |{ <fs_p0022>-zstart DATE = USER }|.
      ENDIF.
      CASE <fs_p0022>-subty.
        WHEN '91'.
          ms_educationadditional-educationstype = '1'.
        WHEN '92'.
          ms_educationadditional-educationstype = '0'.
        WHEN OTHERS.
      ENDCASE.
      CLEAR ms_educationadditional-dpoid.
      SELECT SINGLE stext FROM zthr_dlinet
        INTO ms_educationadditional-dpoid
        WHERE spras EQ 'R' AND
              dline EQ <fs_p0022>-zzdline.
      IF <fs_p0022>-wide_name IS NOT INITIAL.
        ms_educationadditional-institutiontext = <fs_p0022>-wide_name.
      ELSE.
        ms_educationadditional-institutiontext = get_name_insti( iv_insti = <fs_p0022>-insti ).
      ENDIF.
      IF ms_educationadditional-institutiontext IS INITIAL.
        ms_educationadditional-institutiontext = <fs_p0022>-insti.
      ENDIF.

      SELECT SINGLE name FROM zthr_edu_prograt
        INTO ms_educationadditional-speciality
        WHERE lang = 'R'
          AND id   = <fs_p0022>-zznampog.

      SELECT SINGLE stext FROM t519t
        INTO ms_educationadditional-namedocumenteducation
        WHERE sprsl EQ 'R' AND
              slabs EQ <fs_p0022>-slabs.

      ms_educationadditional-educationdocdate	= |{ <fs_p0022>-zzdatdc DATE = USER }|.
      ms_educationadditional-reason = <fs_p0022>-zzprim.
      APPEND ms_educationadditional TO ms_data-educationadditional.
    ENDLOOP.
  ENDMETHOD.
  METHOD fill_certifications.
    DATA:
      lt_obj      TYPE TABLE OF hrsobid
    , lt_profile  TYPE TABLE OF hrpe_profq
    , lt_p0298    TYPE TABLE OF p0298.
    .

    CHECK ms_prop-per08 = abap_true.

    APPEND VALUE #( plvar = '01' otype = 'P' sobid = iv_struc-objid ) TO lt_obj.
    CALL FUNCTION 'RHPP_Q_PROFILE_READ'
      EXPORTING
        begda            = '18000101'
        endda            = '99991231'
*       WITH_STEXT       = 'X'
*       WITH_QK_INFO     = 'X'
*       CHECK_NOTE       = ' '
      TABLES
        objects          = lt_obj
        profile          = lt_profile
      EXCEPTIONS
        no_authority     = 1
        wrong_otype      = 2
        object_not_found = 3
        undefined        = 4
        OTHERS           = 5.

    IF v_cache_attestation IS INITIAL.
      SELECT SINGLE
        low
        INTO v_cache_attestation
        FROM tvarvc
        WHERE name = 'ZZ_HR_ATTESTATION' AND
              type = 'P'.
    ENDIF.
    DATA(lv_vbegd) = ms_prop-begda[ 1 ]-high.
    DELETE lt_profile WHERE class_id NE v_cache_attestation OR vbegd > lv_vbegd.
    SORT lt_profile BY vbegd DESCENDING.
    ASSIGN lt_profile[ 1 ] TO FIELD-SYMBOL(<fs_profile>).
    IF sy-subrc IS INITIAL.
      ms_certifications-personid = iv_struc-objid.
      ms_certifications-date = |{ <fs_profile>-vbegd DATE = USER }|.
      CLEAR ms_certifications-resulttext.
      SELECT tline INTO TABLE @DATA(lt_tline)
        FROM t77sp_descript
        WHERE langu = 'R'
          AND scale_id = '00000090'
          AND rating = @<fs_profile>-profcy.
      LOOP AT lt_tline ASSIGNING FIELD-SYMBOL(<ls_tline>).
        ms_certifications-resulttext = |{ ms_certifications-resulttext }{ <ls_tline>-tline }|.
      ENDLOOP.

      "Идентификатор
      ms_certifications-certificationid = cacheid(
        i_block      = '08'
        i_block_date = <fs_profile>-vbegd
        i_block_type = '0683'
        i_last_date  = ms_prop-begda[ 1 ]-high
        i_personid   = CONV #( iv_struc-objid ) ).

      "Мы получили дату, и теперь забираем из инфотипа данные
      CLEAR lt_p0298.
      mo_get_data->read_pa_infty(
        EXPORTING
          i_pernr = CONV #( iv_struc-objid )
          i_begda = <fs_profile>-vbegd
          i_infty = `0298`
        IMPORTING
          e_pnnnn = lt_p0298  ).

      IF VALUE #( lt_p0298[ massn = 'W3' ]-orddt OPTIONAL ) IS INITIAL.
        ms_certifications-orderdate =  ms_certifications-date .
      ELSE.
        ms_certifications-orderdate   = |{ VALUE #( lt_p0298[ massn = 'W3' ]-orddt OPTIONAL ) DATE = USER }|. "Документ (протокол), дата
      ENDIF.
      ms_certifications-ordernumber = VALUE #( lt_p0298[ massn = 'W3' ]-ordnu OPTIONAL ).                     "Документ (протокол), номер
      IF ms_certifications-ordernumber IS INITIAL.
        ms_certifications-ordernumber = '1'.
      ENDIF.
      DATA(lv_massg) = VALUE #( lt_p0298[ massn = 'W3' ]-massg OPTIONAL ).
      ms_certifications-reason      = VALUE #(                                            "Основание
        t_cache_certifications_reason[ KEY primary_key COMPONENTS massg = lv_massg ]-mgtxt OPTIONAL ).
      IF ms_certifications-reason IS INITIAL AND lv_massg IS NOT INITIAL.
        SELECT SINGLE
          *
          INTO @DATA(ls_t530t)
          FROM t530t
          WHERE sprsl = 'R' AND
                massn = 'W3' AND
                massg = @lv_massg.
        IF ls_t530t IS NOT INITIAL.
          INSERT ls_t530t INTO TABLE t_cache_certifications_reason.
          ms_certifications-reason = ls_t530t-mgtxt.
        ENDIF.
      ENDIF.
      CLEAR lv_massg.

      APPEND ms_certifications TO ms_data-certifications.
    ENDIF.
  ENDMETHOD.
  METHOD fill_holidays.
    DATA: ls_p0298  TYPE p0298
        , lt_p2001  TYPE TABLE OF p2001
        , lv_quonr  TYPE ptquoded-quonr "KLOKOVNYU 19.06.2019
        , ls_p2006   TYPE pa2006  "KLOKOVNYU 19.06.2019
        , lv_date   TYPE char10 "KLOKOVNYU 19.06.2019
        , lv_int TYPE int4 "KLOKOVNYU 28.06.2019
        .

    CHECK ms_prop-per09 = abap_true.

    CLEAR: ms_holidays.
    ms_holidays-personid = iv_struc-objid.
    mo_get_data->read_pa_infty(
      EXPORTING
        i_pernr = CONV #( iv_struc-objid )
        i_begda = ms_prop-begda[ 1 ]-low
        i_endda = ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
        i_infty = `2001`
      IMPORTING
        e_pnnnn = lt_p2001 ).
    DELETE lt_p2001 WHERE sprps = 'X'.
    LOOP AT lt_p2001 ASSIGNING FIELD-SYMBOL(<fs_p2001>) WHERE subty IN s_2001s.
*{ MAKAROVSV 23.09.2019
      ms_holidays-holidayid = cacheid(
        i_block       = '11'
        i_block_date  = <fs_p2001>-begda
        i_block_type  = <fs_p2001>-subty
        i_last_date   = ms_prop-begda[ 1 ]-high
        i_personid    = CONV #( iv_struc-objid ) ).

*} MAKAROVSV 23.09.2019
      CASE <fs_p2001>-subty.
        WHEN '0100'.
          ms_holidays-holidaytype = |ежегодный отпуск|.
        WHEN '0101'.
          ms_holidays-holidaytype = |дополнительный отпуск работникам с ненормированным рабочим днем|.
        WHEN '0102'.
          ms_holidays-holidaytype = |дополнительный отпуск рабочим и служащим, занятым в отдельных отраслях народного хозяйства и имеющим продолжительный стаж работы на одном предприятии, в организации|.
        WHEN '0103' OR '0109' OR '0110' .
          ms_holidays-holidaytype = |отпуск в связи с обучением в вечерних и заочных высших и средних учреждениях профессионального образования|.
        WHEN '0104'.
          ms_holidays-holidaytype = |дополнительный отпуск участникам ликвидации аварий на ЧАЭС и других радиационных аварий|.
        WHEN '0202'.
          ms_holidays-holidaytype = |отпуск по беременности и родам|.
        WHEN '0304'.
          ms_holidays-holidaytype = |дополнительный отпуск за донорство|.
        WHEN '0400' OR '0409' OR '0410' .
          ms_holidays-holidaytype = |отпуск без сохранения заработной платы|.
        WHEN '0501'.
          ms_holidays-holidaytype = |частично оплачиваемый отпуск женщинам, имеющим детей в возрасте до 1,5 лет|.
        WHEN '0502'.
          ms_holidays-holidaytype = |дополнительный отпуск без сохранения заработной платы матерям, имеющим детей в возрасте до 3 лет|.
        WHEN OTHERS.
          ms_holidays-holidaytype = read_subty_text(
            i_subty = <fs_p2001>-subty
          ).
      ENDCASE.

      read_order(
        EXPORTING
          i_pernr = CONV #( iv_struc-objid )
          i_begda = <fs_p2001>-begda
        IMPORTING
          es_p0298 = ls_p0298 ).
      ms_holidays-ordernumb = ls_p0298-ordnu.
      IF ls_p0298-orddt IS NOT INITIAL.
        ms_holidays-orderdate = |{ ls_p0298-orddt DATE = USER }|.
      ELSE.
        CLEAR: ms_holidays-orderdate.
      ENDIF.
*{ KLOKOVNYU 19.06.2019
      IF <fs_p2001>-docnr IS NOT INITIAL.
        SELECT quonr FROM ptquoded INTO lv_quonr WHERE docnr = <fs_p2001>-docnr.
          EXIT.
        ENDSELECT.
        SELECT SINGLE * FROM pa2006 INTO CORRESPONDING FIELDS OF ls_p2006 WHERE quonr = lv_quonr.
        WRITE ls_p2006-begda TO lv_date DD/MM/YYYY.
        ms_holidays-perioddatestart = lv_date.
        CLEAR lv_date.
        WRITE ls_p2006-endda TO lv_date DD/MM/YYYY.
        ms_holidays-perioddateend = lv_date.
        lv_int = ls_p2006-anzhl.
        ms_holidays-periodcountdays = lv_int.
        lv_int = ls_p2006-anzhl.
        SELECT quonr FROM ptquoded INTO lv_quonr WHERE quonr = lv_quonr
                                                   AND datum <= <fs_p2001>-endda.
          lv_int = lv_int - 1.
        ENDSELECT.

        " 15.09.2020   <
        IF lv_int < 0.
          ms_holidays-periodcountdaysremain = |{ '-' }{ abs( lv_int ) }|.
        ELSE.
          ms_holidays-periodcountdaysremain = lv_int.
        ENDIF.

*        ms_holidays-periodcountdaysremain = lv_int.
        " 15.09.2020   >

        IF ms_holidays-holidaytype IS NOT INITIAL.
          WRITE <fs_p2001>-endda TO lv_date DD/MM/YYYY.
          ms_holidays-dateend = lv_date.
        ENDIF.
      ENDIF.
*} KLOKOVNYU 19.06.2019
      lv_int = <fs_p2001>-kaltg.
      ms_holidays-countdays = lv_int.
      ms_holidays-datestart = |{ ls_p0298-begda DATE = USER }|.
      ms_holidays-dateend   = |{ ls_p0298-endda DATE = USER }|.
*{ 01/06/2020
*      IF ms_holidays-perioddatestart IS INITIAL.
*        ms_holidays-perioddatestart = ms_holidays-datestart.
*      ENDIF.
*      IF ms_holidays-perioddateend IS INITIAL.
*        ms_holidays-perioddateend = ms_holidays-dateend.
*      ENDIF.
*      IF ms_holidays-periodcountdays IS INITIAL.
*        ms_holidays-periodcountdays = ms_holidays-countdays.
*      ENDIF.

      IF <fs_p2001>-awart = '0100'.
        ms_holidays-perioddatestart = ms_holidays-perioddatestart.
        ms_holidays-perioddateend = ms_holidays-perioddateend.
        ms_holidays-periodcountdays = ms_holidays-countdays.
        ms_holidays-dateend  = lv_date.
      ELSE.
        IF <fs_p2001> IS ASSIGNED.

          WRITE <fs_p2001>-begda TO lv_date DD/MM/YYYY.
          ms_holidays-perioddatestart = lv_date.
          ms_holidays-datestart       = lv_date   .
          WRITE <fs_p2001>-endda TO lv_date DD/MM/YYYY.
          ms_holidays-perioddateend  = lv_date.
          ms_holidays-dateend = lv_date.

        ENDIF.
        lv_int = <fs_p2001>-kaltg.
        ms_holidays-periodcountdays = lv_int. "<fs_p2001>-kaltg.
        ms_holidays-periodcountdaysremain = '0'.

      ENDIF.
*} 01/06/2020
      APPEND ms_holidays TO ms_data-holidays.
    ENDLOOP.
  ENDMETHOD.
  METHOD fill_orderranks.
*   ИТ 81, считать классные чины   мероприятие p0298-MASSN=ZS
*   OrderRankID
*   PersonID  табельный
*   DateStart	p0081-BEGDA
*   OrderNumb	p0081-ZZRASNU, если пусто то смотреть	p0298-ORDNU по дате
*   OrderDate	p0081-ZZRASDT	p0298-ORDDT
*   RankID  p0081-WDGRD (текст)
    DATA: lt_p0081  TYPE TABLE OF p0081
        , ls_p0298  TYPE p0298
        .

    CHECK ms_prop-per10 = abap_true.
    CLEAR: ms_orderranks.
    mo_get_data->read_pa_infty(
      EXPORTING
        i_pernr = CONV #( iv_struc-objid )
        i_begda = mv_cdate "ms_prop-begda[ 1 ]-high
        i_endda = mv_cdate "ms_prop-begda[ 1 ]-high
        i_infty = `0081`
      IMPORTING
        e_pnnnn = lt_p0081 ).
    ms_orderranks-personid = iv_struc-objid.

    LOOP AT lt_p0081 ASSIGNING FIELD-SYMBOL(<fs_p0081>).
*{ MAKAROVSV 24.09.2019
      ms_orderranks-orderrankid = cacheid(
        i_block      = '13'
        i_block_date = <fs_p0081>-begda
        i_block_type = <fs_p0081>-subty
        i_last_date  = ms_prop-begda[ 1 ]-high
        i_personid   = CONV #( iv_struc-objid ) ).
      ms_orderranks-rankid = read_wdgrd_text( i_wdgrd = <fs_p0081>-wdgrd ).
*} MAKAROVSV 24.09.2019
      ms_orderranks-datestart = |{ <fs_p0081>-begda DATE = USER }|.
      IF <fs_p0081>-zzrasnu IS NOT INITIAL.
        ms_orderranks-ordernumb = <fs_p0081>-zzrasnu.
        ms_orderranks-orderdate = |{ <fs_p0081>-zzrasdt DATE = USER }|.
      ELSE.
        read_order(
          EXPORTING
            i_pernr  = CONV #( iv_struc-objid )
            i_begda  = <fs_p0081>-begda
            i_massn  = 'ZS'
          IMPORTING
            es_p0298 = ls_p0298 ).
        ms_orderranks-ordernumb = ls_p0298-ordnu.
        ms_orderranks-orderdate = |{ ls_p0298-orddt DATE = USER }|.
      ENDIF.

      APPEND ms_orderranks TO ms_data-orderranks.
    ENDLOOP.
  ENDMETHOD.
  METHOD fill_orderdismissals.
    DATA: lt_p0000  TYPE TABLE OF p0000
        , ls_p0298  TYPE p0298
        .

*   PersonID  табельный
*   OrderNumb	ORDNU
*   OrderDate	ORDDT
*   DateStart	BEGDA
*   DismissalReason	MASSG (текст)

    CHECK ms_prop-per11 = abap_true.
    mo_get_data->read_pa_infty(
      EXPORTING
        i_pernr = CONV #( iv_struc-objid )
        i_begda = ms_prop-begda[ 1 ]-low
        i_endda = ms_prop-begda[ 1 ]-high "MAKAROVSV 16.09.2019
        i_infty = `0000`
      IMPORTING
        e_pnnnn = lt_p0000 ).
    CLEAR: ms_orderdismissals.
    ms_orderdismissals-personid = iv_struc-objid.
    LOOP AT lt_p0000 ASSIGNING FIELD-SYMBOL(<fs_p0000>) WHERE massn IN s_mfire.
*{ MAKAROVSV 24.09.2019
      ms_orderdismissals-orderdismissalid = cacheid(
        i_block      = '14'
        i_block_date = <fs_p0000>-begda
        i_block_type = ''
        i_last_date  = ms_prop-begda[ 1 ]-high
        i_personid   = CONV #( iv_struc-objid ) ).
*} MAKAROVSV 24.09.2019
      read_order(
        EXPORTING
          i_pernr  = CONV #( iv_struc-objid )
          i_begda  = <fs_p0000>-begda
          i_massn  = <fs_p0000>-massn
        IMPORTING
          es_p0298 = ls_p0298 ).
      ms_orderdismissals-ordernumb = ls_p0298-ordnu.
      ms_orderdismissals-orderdate = |{ ls_p0298-orddt DATE = USER }|.
      ms_orderdismissals-datestart = |{ <fs_p0000>-begda DATE = USER }|.
*      IF <fs_p0000>-massn = 'Y1'.
*        CASE <fs_p0000>-massg.
*          WHEN '01'.
*            ms_orderdismissals-dismissalreason = |соглашение сторон служебного контракта|.
*          WHEN '02'.
*            ms_orderdismissals-dismissalreason = |расторжение служебного контракта по инициативе гражданского служащего|.
*          WHEN '04'.
*            ms_orderdismissals-dismissalreason = |в связи со смертью (гибелью) гражданского служащего либо признания гражданского служащего безвестно отсутствующим или объявления его умершим решением суда, вступившим в законную силу|.
*          WHEN '05'.
*            ms_orderdismissals-dismissalreason = |истечение срока действия срочного служебного контракта|.
*          WHEN '09'.
*            ms_orderdismissals-dismissalreason = |при неудовлетворительном результате испытания|.
*          WHEN '11'.
*            ms_orderdismissals-dismissalreason = |в связи с призывом гражданского служащего на военную службу или направлением его на альтернативную гражданскую службу|.
*          WHEN '12'.
*            ms_orderdismissals-dismissalreason = |в связи с достижением гражданским служащим предельного возраста пребывания на гражданской службе|.
*          WHEN '13'.
*            ms_orderdismissals-dismissalreason = |несоблюдение ограничений и невыполнение обязательств, установленных 79-ФЗ и другими федеральными законами|.
*          WHEN '17'.
*            ms_orderdismissals-dismissalreason = |в связи с признанием гражданского служащего полностью неспособным к трудовой деятельности в соответствии с медицинским заключением|.
*          WHEN '18'.
*            ms_orderdismissals-dismissalreason = |в связи с восстановлением на службе гражданского служащего, ранее замещавшего эту должность гражданской службы, по решению суда|.
*          WHEN '19'.
*            ms_orderdismissals-dismissalreason = |в связи с осуждением гражданского служащего к наказанию, исключающему возможность замещения должности гражданской службы, по приговору суда, вступившему в законную силу|.
*          WHEN '26'.
*            ms_orderdismissals-dismissalreason = |отказ гражданского служащего от предложенной для замещения иной должности гражданской службы в связи с изменением существенных условий служебного контракта|.
*          WHEN '28'.
*            ms_orderdismissals-dismissalreason = |перевод гражданского служащего по его просьбе или с его согласия в другой государственный орган или на государственную службу иного вида|.
*          WHEN '29'.
*            ms_orderdismissals-dismissalreason = |отказ гражданского служащего от перевода на иную должность гражданской службы по состоянию здоровья в соответствии с медицинским заключением либо отсутствие такой должности|
*            & | в том же государственном органе|.
*          WHEN OTHERS.
*            ms_orderdismissals-dismissalreason = read_massg_text(
*                                             i_massn = <fs_p0000>-massn
*                                             i_massg = <fs_p0000>-massg ).
*        ENDCASE.
*      ELSEIF <fs_p0000>-massn = 'ZN'.
*        CASE <fs_p0000>-massg.
*          WHEN '06'.
*            ms_orderdismissals-dismissalreason = |отказ гражданского служащего от перевода в другую местность вместе с государственным органом|.
*          WHEN OTHERS.
*            ms_orderdismissals-dismissalreason = read_massg_text(
*                                             i_massn = <fs_p0000>-massn
*                                             i_massg = <fs_p0000>-massg ).
*        ENDCASE.
*      ELSE.
*        ms_orderdismissals-dismissalreason = read_massg_text(
*                                               i_massn = <fs_p0000>-massn
*                                               i_massg = <fs_p0000>-massg ).
*      ENDIF.
      ms_orderdismissals-dismissalreason = read_massg_text(
                                             i_massn = <fs_p0000>-massn
                                             i_massg = <fs_p0000>-massg
                                             i_masss = ls_p0298-masss ).
      APPEND ms_orderdismissals TO ms_data-orderdismissals.
    ENDLOOP.
  ENDMETHOD.
  METHOD read_subty_text.
    DATA: ls_subty_cache TYPE ty_subty_cache
        .

    READ TABLE mt_subty_cache WITH TABLE KEY subty = i_subty
      ASSIGNING FIELD-SYMBOL(<fs_subty_cache>).
    IF sy-subrc NE 0.
      SELECT SINGLE atext FROM t554t
        INTO ls_subty_cache-text
        WHERE sprsl EQ 'R' AND
              moabw EQ '33' AND
              awart EQ i_subty.
      IF sy-subrc NE 0.
        RETURN.
      ENDIF.
      ls_subty_cache-subty = i_subty.
      INSERT ls_subty_cache INTO TABLE mt_subty_cache ASSIGNING <fs_subty_cache>.
      CHECK sy-subrc EQ 0.
    ENDIF.

    r_str = <fs_subty_cache>-text.
  ENDMETHOD.
  METHOD read_wdgrd_text.
    DATA: ls_wdgrd_cache TYPE ty_wdgrd_cache
        .
    CASE i_wdgrd.
      WHEN 'B07'.
        r_str = 'Референт государственной гражданской службы Ленинградской области 1 класса'.
      WHEN 'B08'.
        r_str = 'Референт государственной гражданской службы Ленинградской области 2 класса'.
      WHEN 'B09'.
        r_str = 'Референт государственной гражданской службы Ленинградской области 3 класса'.
      WHEN OTHERS.
        READ TABLE mt_wdgrd_cache WITH TABLE KEY wdgrd = i_wdgrd
          ASSIGNING FIELD-SYMBOL(<fs_wdgrd_cache>).
        IF sy-subrc NE 0.
          SELECT SINGLE zztext FROM t554d
            INTO ls_wdgrd_cache-text
            WHERE sprsl EQ 'R' AND
                  wdgrd EQ i_wdgrd.
          IF sy-subrc NE 0.
            RETURN.
          ENDIF.
          ls_wdgrd_cache-wdgrd = i_wdgrd.
          INSERT ls_wdgrd_cache INTO TABLE mt_wdgrd_cache ASSIGNING <fs_wdgrd_cache>.
          CHECK sy-subrc EQ 0.
        ENDIF.
        r_str = <fs_wdgrd_cache>-text.
    ENDCASE.
  ENDMETHOD.
  METHOD read_massg_text.
    DATA: ls_massg_cache TYPE ty_massg_cache
        .
    READ TABLE mt_t7rurst_cache ASSIGNING FIELD-SYMBOL(<fs_t7rurst>)
         WITH TABLE KEY sprsl = sy-langu massn = i_massn massg = i_massg masss = i_masss.
    IF sy-subrc = 0.
      r_str = |{ <fs_t7rurst>-mltxt }{ <fs_t7rurst>-mltx2 }{ <fs_t7rurst>-mltx3 }{ <fs_t7rurst>-mltx4 }|.
    ELSE.
      READ TABLE mt_t7rurst_cache ASSIGNING <fs_t7rurst>
           WITH KEY sprsl = sy-langu massn = i_massn massg = i_massg .
      IF sy-subrc = 0.
        r_str = |{ <fs_t7rurst>-mltxt }{ <fs_t7rurst>-mltx2 }{ <fs_t7rurst>-mltx3 }{ <fs_t7rurst>-mltx4 }|.
      ELSE.
        READ TABLE mt_massg_cache ASSIGNING FIELD-SYMBOL(<fs_massg_cache>)
          WITH TABLE KEY massn = i_massn massg = i_massg.

        IF sy-subrc NE 0.
          SELECT SINGLE mgtxt FROM t530t
            INTO ls_massg_cache-text
            WHERE sprsl EQ 'R' AND
                  massn EQ i_massn AND
                  massg EQ i_massg.
          IF sy-subrc NE 0.
            RETURN.
          ENDIF.

          ls_massg_cache-massn = i_massn.
          ls_massg_cache-massg = i_massg.
          INSERT ls_massg_cache INTO TABLE mt_massg_cache ASSIGNING <fs_massg_cache>.
          CHECK sy-subrc EQ 0.
        ENDIF.

        r_str = <fs_massg_cache>-text.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD read_order.
    DATA: lt_p0298 TYPE TABLE OF p0298
        .

    CLEAR: es_p0298.
    mo_get_data->read_pa_infty(
      EXPORTING
        i_pernr = i_pernr
        i_begda = i_begda
        i_infty = `0298`
      IMPORTING
        e_pnnnn = lt_p0298 ).

    IF i_massn IS NOT INITIAL.
      DELETE lt_p0298 WHERE massn NE i_massn.
    ENDIF.

    READ TABLE lt_p0298 INDEX 1 INTO es_p0298.
  ENDMETHOD.
  METHOD read_p1000_data.
    DATA: lt_p1000 TYPE TABLE OF p1000
        , lv_datum TYPE text10
        .
    mo_get_data->read_om_infty( EXPORTING i_otype = iv_otype
                                          i_objid = iv_objid
                                          i_infty = `1000`
                                          i_endda = ms_prop-begda[ 1 ]-high
                                IMPORTING e_pnnnn = lt_p1000 ).

    SORT lt_p1000 BY begda.

    LOOP AT lt_p1000 ASSIGNING FIELD-SYMBOL(<lt_p1000>).
      "Дата создания
      IF ev_datum IS INITIAL.
        WRITE <lt_p1000>-begda TO lv_datum LEFT-JUSTIFIED.
        ev_datum = lv_datum.
      ENDIF.
      CHECK <lt_p1000>-langu = sy-langu.
      "Код подразделения
      ev_code  = <lt_p1000>-short.
      "Короткое название подразделения
      ev_sname = <lt_p1000>-stext.
    ENDLOOP.
  ENDMETHOD.
  METHOD read_p1005_data.
    DATA: lt_p1005    TYPE TABLE OF p1005
        .
    mo_get_data->read_om_infty( EXPORTING i_otype = iv_otype
                                          i_objid = iv_objid
                                          i_infty = '1005'
                                          i_begda = ms_prop-begda[ 1 ]-high
                                          i_endda = ms_prop-begda[ 1 ]-high
                                 IMPORTING e_pnnnn = lt_p1005 ).

    LOOP AT lt_p1005 ASSIGNING FIELD-SYMBOL(<lt_p1005>).
      ev_cpmin = <lt_p1005>-cpmin.
    ENDLOOP.
  ENDMETHOD.
  METHOD sen_calculate.
    CALL FUNCTION 'HR_SEN_CALCULATE_COMPLETE'
      EXPORTING
        id_pernr                 = iv_pernr
        id_molga                 = '33'
        id_proce                 = iv_proce
        id_sel_date              = iv_begda
      IMPORTING
        es_duration              = rs_durat
      EXCEPTIONS
        wrong_import_parameter   = 1
        wrong_calculation_type   = 2
        process_not_found        = 3
        process_pstep_not_found  = 4
        no_result_for_calty      = 5
        error_of_other_functions = 6
        OTHERS                   = 7.
  ENDMETHOD.
  METHOD fill_education_grad.
    DATA: lt_p0022 TYPE TABLE OF p0022
         ,lt_t7ruokso TYPE TABLE OF t7ruokso
         ,lv_subty TYPE subty
         ,lr_subty LIKE RANGE OF lv_subty
         ,lt_p0022_collect TYPE TABLE OF p0022.

    CHECK ms_prop-per13 = abap_true.

    lr_subty = VALUE #( ( sign = 'I' option = 'EQ' low = '19' ) ).

    LOOP AT lr_subty INTO DATA(ls_subty).
      mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
                                            i_begda = mv_cdate "ms_prop-begda[ 1 ]-high
                                            i_endda = mv_cdate "ms_prop-begda[ 1 ]-high
                                            i_infty = `0022`
                                            i_subty = ls_subty-low
                                  IMPORTING e_pnnnn = lt_p0022 ).
      APPEND LINES OF lt_p0022 TO lt_p0022_collect.
    ENDLOOP.
    lt_p0022 = lt_p0022_collect.

    SORT: lt_p0022 BY begda.

    LOOP AT lt_p0022 ASSIGNING FIELD-SYMBOL(<lt_p0022>) WHERE NOT subty IN ms_prop-sub22.
      CLEAR: ms_education_grad
           .
      ms_education_grad-educationgraduateid   = cacheid(
          i_personid    = CONV #( iv_struc-objid )
          i_block       = '03'
          i_block_type  = <lt_p0022>-subty
          i_block_date  = <lt_p0022>-begda
          i_last_date   = ms_prop-begda[ 1 ]-high ).                   "Идентификатор
      ms_education_grad-personid              = iv_struc-objid .       "ИдентийикаторИдентификатор сотрудника
*      ms_education_grad-educationokin         = <lt_p0022>-slart.      "ИдентийикаторОбразование ОКИН

      "ИдентийикаторНаименование образовательного учреждения
      IF <lt_p0022>-wide_name IS NOT INITIAL.
        ms_education_grad-institutiontext = <lt_p0022>-wide_name.
      ELSE.
        ms_education_grad-institutiontext = get_name_insti( iv_insti = <lt_p0022>-insti ).
      ENDIF.
      IF ms_education_grad-institutiontext  IS INITIAL.
        ms_education_grad-institutiontext  = <lt_p0022>-insti.
      ENDIF.
      "ИдентийикаторГод начала
      IF <lt_p0022>-zstart IS NOT INITIAL.
        ms_education_grad-datestart = <lt_p0022>-zstart+0(4).
      ENDIF.
      "ИдентийикаторГод окончания
      IF <lt_p0022>-zend IS NOT INITIAL.
        ms_education_grad-enddate = <lt_p0022>-zend(4).
      ELSEIF <lt_p0022>-zzdatdc IS NOT INITIAL.
        ms_education_grad-enddate = <lt_p0022>-zzdatdc(4).
      ENDIF.
      "ИдентийикаторНаименование документа об образовании
      ms_education_grad-namedocumenteducation = <lt_p0022>-slabs.
      SELECT SINGLE
        stext
        INTO ms_education_grad-namedocumenteducation
        FROM t519t
        WHERE slabs = <lt_p0022>-slabs
          AND sprsl = 'R'.

      IF <lt_p0022>-zzshort IS NOT INITIAL.
        DATA lt_cocso TYPE RANGE OF t7ruokso-cokso.
        CLEAR lt_cocso.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <lt_p0022>-zzshort ) TO lt_cocso.
        DATA lv_cocso TYPE t7ruokso-cokso.
        lv_cocso = <lt_p0022>-zzshort.
        DO strlen( <lt_p0022>-zzshort ) TIMES.
          IF lv_cocso(1) = '0'.
            lv_cocso = lv_cocso+1.
            APPEND VALUE #( sign = 'I' option = 'EQ' low = lv_cocso ) TO lt_cocso.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.

        DATA lv_data TYPE d.
        lv_data = ms_prop-begda[ 1 ]-high.
        SELECT *
          FROM t7ruokso
          INTO TABLE lt_t7ruokso
          WHERE cokso IN lt_cocso
            AND endda >= lv_data
            AND begda <= lv_data
        .
        SORT lt_t7ruokso BY cokso.
        READ TABLE lt_t7ruokso INTO DATA(ls_t7ruokso) INDEX 1.
        ms_education_grad-speciality = ls_t7ruokso-cname.
      ENDIF.

      ms_education_grad-educationdocdate = |{ <lt_p0022>-zzdatdc DATE = USER }|.

      "Номер документа о послевузовском профессиональном образовании
      IF <lt_p0022>-zzseria IS INITIAL.
        ms_education_grad-educationdocnumber = <lt_p0022>-ksbez.
      ELSE.
        ms_education_grad-educationdocnumber = |{ <lt_p0022>-zzseria } { <lt_p0022>-ksbez }|.
        CONDENSE ms_education_grad-educationdocnumber.
      ENDIF.
      IF ms_education_grad-educationdocnumber IS INITIAL.
        ms_education_grad-educationdocnumber = '0'.
      ENDIF.

      "Ученая степень
      SELECT SINGLE cname
        INTO ms_education_grad-academicdegree
        FROM t7ruokin
        WHERE molga = '33' AND
              sprsl = 'RU' AND
              facet = '35' AND
              ccode = <lt_p0022>-zzstulv.

      SELECT SINGLE
        cname
        INTO ms_education_grad-education
        FROM t7ruokin
        WHERE molga = '33'
          AND sprsl = 'R'
          AND facet = '34'
          AND ccode = <lt_p0022>-slktr.

      "ИдентийикаторНаправление или специальность по документу код по ОКСО
      ms_education_grad-specialityokso         = <lt_p0022>-zzshort.

      COLLECT ms_education_grad INTO  ms_data-education_grad.
    ENDLOOP.
  ENDMETHOD.
  METHOD fill_orderaward.
    DATA: lt_p0183 TYPE TABLE OF p0183,
          lv_date  TYPE char10.

    CHECK ms_prop-per14  = abap_true.

    mo_get_data->read_pa_infty( EXPORTING i_pernr = CONV #( iv_struc-objid )
                                          i_begda = mv_cdate "ms_prop-begda[ 1 ]-high
                                          i_endda = mv_cdate "ms_prop-begda[ 1 ]-high
                                          i_infty = `0183`
                                IMPORTING e_pnnnn = lt_p0183 ).
    SORT: lt_p0183 BY begda.

    LOOP AT lt_p0183 ASSIGNING FIELD-SYMBOL(<lt_p0183>) WHERE awdtp IN s_0183s.
      CLEAR: ms_orderaward.

      "Идентификатор
      ms_orderaward-orderawardid = cacheid(
        i_block       = '10'
        i_block_date  = <lt_p0183>-begda
        i_block_type  = <lt_p0183>-awdtp
        i_last_date   = ms_prop-begda[ 1 ]-high
        i_personid    = CONV #( iv_struc-objid ) ).

      "Тип награждения
      ms_orderaward-awardkind = COND #(
        WHEN <lt_p0183>-awdtp CP '2*' OR
             <lt_p0183>-awdtp CP '3*' OR
             <lt_p0183>-awdtp CP '4*' OR
             <lt_p0183>-awdtp CP '5*' OR
             <lt_p0183>-awdtp CP '6*' OR
             <lt_p0183>-awdtp CP 'A*' OR
             <lt_p0183>-awdtp CP 'B*'
        THEN 'государственные награды'
        WHEN <lt_p0183>-awdtp CP '7*' OR
             <lt_p0183>-awdtp CP '8*'
        THEN 'ведомственные награды'
        WHEN <lt_p0183>-awdtp CP 'Z*'
        THEN 'иные'
      ).
      ms_orderaward-personid = iv_struc-objid.
      ms_orderaward-awardtype = COND #(
        WHEN <lt_p0183>-awdtp CP '2*' OR
             <lt_p0183>-awdtp CP '3*'
        THEN |награждение орденами и медалями Российской Федерации|
        WHEN <lt_p0183>-awdtp CP '4*'
        THEN |награждение знаками отличия Российской Федерации|
        WHEN <lt_p0183>-awdtp CP '6*'
        THEN |присвоение почетных званий Российской Федерации|
        WHEN <lt_p0183>-awdtp = '7655' OR
             <lt_p0183>-awdtp = '7672' OR
             <lt_p0183>-awdtp = '7675'
        THEN |награждение почетной грамотой государственного ор| &&
             |гана с выплатой единовременного поощрения или с вручением ценного подарка|
        ELSE |иные виды поощрений|
      ).
      SELECT SINGLE awdtx FROM zthr_t5r02 INTO ms_orderaward-awardname
        WHERE awdtp = <lt_p0183>-awdtp.
      SELECT SINGLE stext FROM zthr_acttyt INTO ms_orderaward-namedocumentaward
        WHERE actty = <lt_p0183>-zzactty.
      ms_orderaward-numberawarding = <lt_p0183>-zzactnu.
      ms_orderaward-dateawarding = <lt_p0183>-zzactdt.

      WRITE <lt_p0183>-zzactdt TO lv_date DD/MM/YYYY.
      ms_orderaward-dateawarding = lv_date.
      COLLECT ms_orderaward INTO  ms_data-orderaward.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_abolitiondate.
    DATA: lt_p1014 TYPE TABLE OF p1014.

* -- Дата упразднения
    CALL FUNCTION 'RH_READ_INFTY'
      EXPORTING
        authority            = ' '
        with_stru_auth       = ' '
        plvar                = '01'
        otype                = 'O'
        objid                = i_objid
        infty                = '1014'
        begda                = i_begda
        endda                = i_endda
      TABLES
        innnn                = lt_p1014
      EXCEPTIONS
        all_infty_with_subty = 1
        nothing_found        = 2
        no_objects           = 3
        wrong_condition      = 4
        wrong_parameters     = 5
        OTHERS               = 6.
    IF sy-subrc = 0.
      READ TABLE lt_p1014 ASSIGNING FIELD-SYMBOL(<fs_p0014>) WITH KEY redun = abap_true.
      IF sy-subrc EQ 0.
        IF <fs_p0014>-begda IS NOT INITIAL.
          r_begda = |{ <fs_p0014>-begda DATE = USER }|.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  METHOD get_citizenshipokin.
    SELECT SINGLE
      b~cname
      INTO r_str
      FROM t7runatio AS a
      JOIN t7ruokin  AS b ON a~natio = b~ccode
      WHERE a~nati1 = i_natio AND
            a~sprsl = 'RU'    AND
            b~molga = '33'    AND
            b~sprsl = 'RU'    AND
            b~facet = '02'
    .
  ENDMETHOD.
  METHOD get_substitutepersonid.
    DATA: lt_p1001 TYPE TABLE OF p1001 .

    mo_get_data->read_om_infty(
      EXPORTING
        i_plvar     = '01'
        i_otype     = 'P'
        i_objid     = i_objid    " Идентификатор объекта
        i_infty     = '1001'     " Инфо-тип
        i_authority = space      " Общий флаг
        i_sauth     = space      " Общий флаг
        i_begda     = i_endda
        i_endda     = i_endda
      IMPORTING
        e_pnnnn     = lt_p1001 ).

    READ TABLE lt_p1001 INTO DATA(wa_p1001) WITH KEY sclas = 'S'.
    IF sy-subrc IS INITIAL.
      CLEAR lt_p1001.
      mo_get_data->read_om_infty(
        EXPORTING
          i_plvar     = '01'
          i_otype     = 'S'
          i_objid     = CONV #( wa_p1001-sobid )
          i_infty     = '1001'
          i_authority = space
          i_sauth     = space
          i_begda     = i_endda
          i_endda     = i_endda
        IMPORTING
          e_pnnnn     = lt_p1001 ).

      DELETE lt_p1001 WHERE sclas <> 'P'.
      DESCRIBE TABLE lt_p1001 LINES DATA(lv_count_rows).
      IF lv_count_rows >= 2.
        DELETE lt_p1001 WHERE prozt = '100' AND sobid = i_objid .
        DESCRIBE TABLE lt_p1001 LINES lv_count_rows.
        IF lv_count_rows = 1.
          READ TABLE lt_p1001 INDEX 1 ASSIGNING FIELD-SYMBOL(<wa_p1001>) .
          IF <wa_p1001>-sobid <> i_objid.
            r_sobid = <wa_p1001>-sobid.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    DATA lv_prozt TYPE p LENGTH 3 DECIMALS 2.
    lv_prozt = wa_p1001-prozt  / 100.
    e_prozt  = lv_prozt.
    REPLACE '.' IN e_prozt WITH ','.
  ENDMETHOD.
  METHOD cacheid.
    DATA lv_rc TYPE inri-returncode.
    READ TABLE t_cache_cacheid WITH TABLE KEY personid COMPONENTS personid = i_personid TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      SELECT
        *
        INTO TABLE @t_cache_cacheid
        FROM zthr_ess_d001
        WHERE personid = @i_personid.
    ENDIF.

    DATA(ls_row) = VALUE #( t_cache_cacheid[
      KEY primary_key COMPONENTS personid   = i_personid
                                 block      = i_block
                                 block_type = i_block_type
                                 block_date = i_block_date ] OPTIONAL ).
    IF ls_row IS INITIAL.
      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr             = '01'
          object                  = 'ZHR_ESS'
          subobject               = i_block
        IMPORTING
          number                  = ls_row-block_id
          returncode              = lv_rc
        EXCEPTIONS
          interval_not_found      = 1
          number_range_not_intern = 2
          object_not_found        = 3
          quantity_is_0           = 4
          quantity_is_not_1       = 5
          interval_overflow       = 6
          buffer_overflow         = 7
          OTHERS                  = 8.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      ls_row-mandt      = sy-mandt.
      ls_row-personid   = i_personid.
      ls_row-block      = i_block.
      ls_row-block_type = i_block_type.
      ls_row-block_date = i_block_date.
    ENDIF.

    ls_row-last_date = i_last_date.

    MODIFY zthr_ess_d001 FROM @ls_row.
    COMMIT WORK.

    INSERT ls_row INTO TABLE t_cache_cacheid.

    r_block_id = ls_row-block_id.
  ENDMETHOD.
  METHOD fill_socialsecurity.
    DATA:
      lt_p9003 TYPE TABLE OF p9003,
      lt_obj   TYPE hrobject_t.

    CHECK ms_prop-per15 = abap_true.

    APPEND VALUE #( plvar = '01' otype = 'P' objid = iv_struc-objid ) TO lt_obj.
    mo_get_data->read_om_infty(
      EXPORTING
        i_objid     = CONV #( iv_struc-objid )    " Идентификатор объекта
        i_infty     = '9003'                      " Инфо-тип
        i_authority = space                       " Общий флаг
        i_sauth     = space                       " Общий флаг
        i_object    = lt_obj
        i_begda     = mv_cdate "ms_prop-begda[ 1 ]-low
        i_endda     = mv_cdate "ms_prop-begda[ 1 ]-high
      IMPORTING
        e_pnnnn     = lt_p9003 ).
    LOOP AT lt_p9003 ASSIGNING FIELD-SYMBOL(<wa_p9003>).
      CLEAR ms_socialsecurity.

      ms_socialsecurity-socialsecurityid = cacheid(         "Идентификатор
          i_block       = '12'
          i_block_date  = <wa_p9003>-begda
          i_block_type  = <wa_p9003>-subty
          i_last_date   = ms_prop-begda[ 1 ]-high
          i_personid    = CONV #( iv_struc-objid ) ).

      ms_socialsecurity-personid  = CONV #( iv_struc-objid )."Идентификатор сотрудника

      READ TABLE t_cache_socialsecurity INTO DATA(wa_socialbenefit) WITH TABLE KEY primary_key COMPONENTS infty = <wa_p9003>-infty subty = <wa_p9003>-subty.
      IF sy-subrc IS NOT INITIAL.
        SELECT SINGLE
          *
          INTO @wa_socialbenefit
          FROM t591s
          WHERE sprsl = 'R' AND
                infty = @<wa_p9003>-infty AND
                subty = @<wa_p9003>-subty.
        IF wa_socialbenefit IS NOT INITIAL.
          INSERT wa_socialbenefit INTO TABLE t_cache_socialsecurity.
        ENDIF.
      ENDIF.
      ms_socialsecurity-socialbenefit = wa_socialbenefit-stext.
      CLEAR wa_socialbenefit.
      ms_socialsecurity-docnumber     = <wa_p9003>-zznomer.
      ms_socialsecurity-datestart     = |{ <wa_p9003>-zzdatbg DATE = USER }|.
      ms_socialsecurity-dateend       = |{ <wa_p9003>-zzdaten DATE = USER }|.
      ms_socialsecurity-reason        = <wa_p9003>-zzreason1 && <wa_p9003>-zzreason2.

      COLLECT ms_socialsecurity INTO  ms_data-socialsecurity.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_common_hascertification.
    DATA: lt_obj  TYPE TABLE OF hrsobid
        , lt_profile TYPE TABLE OF hrpe_profq
        , lt_p0298  TYPE TABLE OF p0298.

    APPEND VALUE #( plvar = '01' otype = 'P' sobid = iv_struc-objid ) TO lt_obj.
    CALL FUNCTION 'RHPP_Q_PROFILE_READ'
      EXPORTING
        begda            = ms_prop-begda[ 1 ]-high
        endda            = ms_prop-begda[ 1 ]-high
      TABLES
        objects          = lt_obj
        profile          = lt_profile
      EXCEPTIONS
        no_authority     = 1
        wrong_otype      = 2
        object_not_found = 3
        undefined        = 4
        OTHERS           = 5.

    IF v_cache_attestation IS INITIAL.
      SELECT SINGLE
        low
        INTO v_cache_attestation
        FROM tvarvc
        WHERE name = 'ZZ_HR_ATTESTATION' AND
              type = 'P'.
    ENDIF.

    DELETE lt_profile WHERE class_id NE v_cache_attestation .
    IF lt_profile IS NOT INITIAL.
      r_res = 1.
    ELSE.
      r_res = 0.
    ENDIF.
  ENDMETHOD.
  METHOD get_common_hasadditionaleducat.
    DATA: lt_p0022 TYPE TABLE OF p0022
      .
    mo_get_data->read_pa_infty(
      EXPORTING
        i_pernr = CONV #( iv_struc-objid )
        i_begda = ms_prop-begda[ 1 ]-high
        i_endda = ms_prop-begda[ 1 ]-high
        i_infty = `0022`
      IMPORTING
        e_pnnnn = lt_p0022 ).
    DELETE lt_p0022 WHERE subty NE '91' AND subty NE '92'.
    IF lt_p0022 IS NOT INITIAL.
      r_res = 1.
    ELSE.
      r_res = 0.
    ENDIF.
  ENDMETHOD.
  METHOD get_common_hassocialbenefit.
    DATA:
      lt_p9003 TYPE TABLE OF p9003,
      lt_obj   TYPE hrobject_t.

    APPEND VALUE #( plvar = '01' otype = 'P' objid = iv_struc-objid ) TO lt_obj.
    mo_get_data->read_om_infty(
      EXPORTING
        i_objid     = CONV #( iv_struc-objid )
        i_infty     = '9003'
        i_authority = space
        i_sauth     = space
        i_object    = lt_obj
        i_begda     = ms_prop-begda[ 1 ]-low
        i_endda     = ms_prop-begda[ 1 ]-high
      IMPORTING
        e_pnnnn     = lt_p9003 ).
    IF lt_p9003 IS NOT INITIAL .
      r_res = 1.
    ELSE.
      r_res = 0.
    ENDIF.
  ENDMETHOD.
  METHOD get_common_hasaward.
    DATA: lt_p0183 TYPE TABLE OF p0183.

    mo_get_data->read_pa_infty(
      EXPORTING i_pernr = CONV #( iv_struc-objid )
                i_begda = ms_prop-begda[ 1 ]-high
                i_endda = ms_prop-begda[ 1 ]-high
                i_infty = `0183`
      IMPORTING e_pnnnn = lt_p0183 ).
    IF lt_p0183 IS NOT INITIAL.
      r_res = 1.
    ELSE.
      r_res = 0.
    ENDIF.
  ENDMETHOD.
  METHOD get_objid_struc.
    DATA lt_struc1 TYPE struc_t .

*    mo_get_data->get_objid_struc(
*      EXPORTING
*        i_otype = ms_prop-otype
*        i_objid = i_objid
*        i_wegid = i_wegid
*        i_begda = mv_cdate "ms_prop-begda[ 1 ]-high
*        i_vflag = abap_true
*      IMPORTING
*        e_struc = rt_struc ).

    "Требование чтобы можно было получать орг.структуру за период
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype      = ms_prop-otype
        act_objid      = i_objid
        act_wegid      = i_wegid
        act_begda      = CONV objec-begda( ms_prop-begda[ 1 ]-high )
        act_endda      = CONV objec-endda( ms_prop-begda[ 1 ]-high )
      TABLES
        result_struc   = rt_struc
      EXCEPTIONS
        no_plvar_found = 1
        no_entry_found = 2
        OTHERS         = 3.

    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype      = ms_prop-otype
        act_objid      = i_objid
        act_wegid      = i_wegid
        act_begda      = CONV objec-begda( ms_prop-begda[ 1 ]-low )
        act_endda      = CONV objec-endda( ms_prop-begda[ 1 ]-high )
      TABLES
        result_struc   = lt_struc1
      EXCEPTIONS
        no_plvar_found = 1
        no_entry_found = 2
        OTHERS         = 3.

    DELETE lt_struc1 WHERE otype <> 'P'.

    "Нужно исключить указанные ветки из общей структуры
    DATA lt_struc LIKE rt_struc.
    IF mt_excluded_objid IS NOT INITIAL.
      LOOP AT rt_struc ASSIGNING FIELD-SYMBOL(<ls_struc>).
        READ TABLE mt_excluded_objid TRANSPORTING NO FIELDS
          WITH TABLE KEY primary_key COMPONENTS table_line = <ls_struc>-objid.
        IF sy-subrc IS INITIAL.
          APPEND <ls_struc> TO lt_struc.
        ENDIF.
      ENDLOOP.
      LOOP AT lt_struc ASSIGNING <ls_struc>.
        delete_excluded_path(
          EXPORTING
            is_struc = <ls_struc>
          CHANGING
            ct_struc = rt_struc
        ).
      ENDLOOP.
    ENDIF.

    "Нужно исключить людей которые уже в системе, но дата официального начала их работы не наступила
    "Для случая поиска от человека к орг. единицам, в обратном порядке нет необходимости
    IF lines( rt_struc ) = 1.
      IF rt_struc[ 1 ]-otype = 'P'.
        CLEAR rt_struc.
      ENDIF.
    ENDIF.
    " добавление уволенных в основную таблицу
    SORT rt_struc BY otype objid.
    DELETE ADJACENT DUPLICATES FROM lt_struc1 COMPARING otype objid.

    LOOP AT rt_struc ASSIGNING <ls_struc>.
      READ TABLE lt_struc1 ASSIGNING FIELD-SYMBOL(<ls_row>) WITH KEY objid = <ls_struc>-objid BINARY SEARCH.
      IF sy-subrc = 0.
        CLEAR <ls_row>-objid.
      ENDIF.
    ENDLOOP.

    DELETE lt_struc1 WHERE objid IS INITIAL.

    " проверим статус на конец периода - исключим переводы
    LOOP AT lt_struc1 ASSIGNING <ls_row> .
      me->read_p0000( iv_pernr = CONV #( <ls_row>-objid ) ). " 05/06/2020
      IF ms_p0000-stat2 <> '0'.
        CLEAR <ls_row>-objid.

      ENDIF.
    ENDLOOP.

    DELETE lt_struc1 WHERE objid IS INITIAL.

    IF lines( lt_struc1 ) > 0 .
      APPEND LINES OF lt_struc1 TO rt_struc.
    ENDIF.

    " 13.08.2020   <
*    SORT rt_struc BY otype objid.
*    DELETE ADJACENT DUPLICATES FROM rt_struc COMPARING otype objid.
    " 13.08.2020   >

  ENDMETHOD.
  METHOD delete_excluded_path.
    DATA lt_struc LIKE ct_struc.
    LOOP AT ct_struc ASSIGNING FIELD-SYMBOL(<ls_struc>) WHERE pup = is_struc-seqnr.
      APPEND <ls_struc> TO lt_struc.
    ENDLOOP.

    DELETE ct_struc WHERE seqnr = is_struc-seqnr.

    LOOP AT lt_struc ASSIGNING <ls_struc>.
      delete_excluded_path(
        EXPORTING
          is_struc = <ls_struc>
        CHANGING
          ct_struc = ct_struc
      ).
    ENDLOOP.
  ENDMETHOD.
  METHOD get_excluded_objid.
    DATA lt_objid LIKE ms_prop-objid.
    LOOP AT ms_prop-objid INTO DATA(ls_objid).
      IF ls_objid-sign = 'E'.
        ls_objid-sign = 'I'.
        INSERT ls_objid INTO TABLE lt_objid.
      ENDIF.
    ENDLOOP.

    IF lt_objid IS NOT INITIAL.
      DATA(lv_date) = ms_prop-begda[ 1 ]-high.
      SELECT
        objid
        INTO TABLE @rt_objid
        FROM hrp1000
        WHERE otype =  'O'       AND
              plvar =  @i_plvar  AND
              objid IN @lt_objid AND
              begda <= @lv_date  AND
              endda >= @lv_date
        GROUP BY objid.
    ENDIF.
  ENDMETHOD.
  METHOD get_data_for_cache.
    CLEAR t_cache_cacheid.

    "Если выбранно: Образование
    IF ms_prop-per02 = abap_true.
      get_education_old_3x( ).
    ENDIF.

    "Если выбранно: Состав семьи
    IF ms_prop-per06 = abap_true.
      CLEAR mt_t502t.
      SELECT * FROM t502t INTO TABLE mt_t502t WHERE sprsl = sy-langu.
    ENDIF.

    DATA(lv_date) = ms_prop-begda[ 1 ]-high.

    SELECT * INTO CORRESPONDING FIELDS OF TABLE mt_t7rurst_cache
      FROM  t7rurst
     WHERE sprsl = sy-langu
       AND endda >= lv_date.

  ENDMETHOD.
  METHOD get_education_old_3x.
    CLEAR t_cache_education_3x.

    DATA(lv_objid) = zcl_hrpa_tvarvc=>read_parameter( i_name = 'ZZ_HR_EDUC_MANUALLY' ).

    CHECK lv_objid IS NOT INITIAL.

    DATA: lt1001 TYPE STANDARD TABLE OF p1001,
          ls1001 TYPE p1001.
    CALL FUNCTION 'RH_READ_INFTY_1001'
      EXPORTING
        plvar            = CONV plog-plvar( ms_prop-plvar )
        otype            = '3R'
        objid            = CONV plog-objid( lv_objid )
        begda            = CONV plog-begda( ms_prop-begda[ 1 ]-high )
        endda            = CONV plog-endda( ms_prop-begda[ 1 ]-high )
      TABLES
        i1001            = lt1001
      EXCEPTIONS
        nothing_found    = 1
        wrong_condition  = 2
        wrong_parameters = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE text-e02 TYPE 'E' DISPLAY LIKE 'E'.
    ENDIF.

    DELETE lt1001 WHERE sclas <> '3U'.

    DATA:
        lt_objec TYPE objec_t
      , ls_objec LIKE LINE OF lt_objec
      .
    LOOP AT lt1001 INTO ls1001.
      CALL FUNCTION 'RH_STRUC_GET'
        EXPORTING
          act_otype      = '3U'
          act_objid      = CONV objec-objid( ls1001-sobid )
          act_wegid      = 'Z_OKSO'
          act_plvar      = CONV objec-plvar( ms_prop-plvar )
          act_begda      = CONV objec-begda( ms_prop-begda[ 1 ]-high )
          act_endda      = CONV objec-endda( ms_prop-begda[ 1 ]-high )
        TABLES
          result_objec   = lt_objec
        EXCEPTIONS
          no_plvar_found = 1
          no_entry_found = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        MESSAGE text-e02 TYPE 'E' DISPLAY LIKE 'E'.
      ENDIF.

      DELETE lt_objec WHERE otype <> '3X'.

      LOOP AT lt_objec INTO ls_objec.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_objec-objid ) TO t_cache_education_3x.
      ENDLOOP.
      CLEAR lt_objec.
    ENDLOOP.
  ENDMETHOD.
  METHOD check_oe_has_free_oe.
    DATA lt_p1008 TYPE TABLE OF p1008.
    CALL FUNCTION 'RH_READ_INFTY_NNNN'
      EXPORTING
        infty                 = '1008'
        plvar                 = CONV plog-plvar( ms_prop-plvar )
        otype                 = 'O'
        objid                 = CONV plog-objid( iv_objid )
        begda                 = CONV plog-begda( ms_prop-begda[ 1 ]-high )
        endda                 = CONV plog-endda( ms_prop-begda[ 1 ]-high )
      TABLES
        innnn                 = lt_p1008
      EXCEPTIONS
        nothing_found         = 1
        wrong_condition       = 2
        infotyp_not_supported = 3
        wrong_parameters      = 4
        OTHERS                = 5.
    LOOP AT lt_p1008 ASSIGNING FIELD-SYMBOL(<ls_p1008>).
      IF
        <ls_p1008>-persa = '0160' OR
        <ls_p1008>-persa = '0161' OR
        <ls_p1008>-persa = '0162' OR
        <ls_p1008>-persa = '0163' OR
        <ls_p1008>-persa = '0164' OR
        <ls_p1008>-persa = '0165'
        .

        rv_res = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD summ_experiens.
    DATA lv_int1 TYPE i.
    DATA lv_int2 TYPE i.

    lv_int1 = 12 * 30 * CONV i( is_exp1-calyy ) + 30 * CONV i( is_exp1-calmm ) + CONV i( is_exp1-caldd ).
    lv_int2 = 12 * 30 * CONV i( is_exp2-calyy ) + 30 * CONV i( is_exp2-calmm ) + CONV i( is_exp2-caldd ).

    lv_int1 = lv_int1 + lv_int2.

    rs_durat-calyy = lv_int1 DIV ( 12 * 30 ).
    rs_durat-calmm = ( lv_int1 MOD ( 12 * 30 ) ) DIV 30.
    rs_durat-caldd = ( lv_int1 MOD ( 12 * 30 ) ) MOD 30.
  ENDMETHOD.
  METHOD get_name_insti.
    SELECT SINGLE
      *
      INTO @DATA(ls_t7ruschool)
      FROM t7ruschool
      WHERE molga = '33'
        AND sprsl = @sy-langu
        AND abbrv = @iv_insti.

    rv_res = ls_t7ruschool-insti.
  ENDMETHOD.
ENDCLASS.
