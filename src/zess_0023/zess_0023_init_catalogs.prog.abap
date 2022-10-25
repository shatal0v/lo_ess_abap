*&---------------------------------------------------------------------*
*& Report  ZESS_0023_INIT_CATALOGS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZESS_0023_INIT_CATALOGS.

*Программа заполнения справочников.
*Написана почти по принуждению, поскольку ни одно GUI средство не
*дает внести текст длинной более 127 символов.
*Каждый справочник перед заполнением очищается


*Код заполнения закоментарен, чтобы не затереть ничего
*                       ВНИМАНИЕ!!!!
*РАСКОММЕНТИРОВАНИЕ И ЗАПУСК МОЖЕТ УНИТОЖИТЬ ВАЖНЫЕ ДАННЫЕ
*                  ВЫЙДИ БЕЗ СОХРАНЕНИЯ

*============== Справочник "Ожидаемая результативность" ===============

***DATA:
***  lt_efct TYPE TABLE OF zess_0023_t_efct,
***  ls_efct TYPE zess_0023_t_efct.
***FIELD-SYMBOLS: <ls_efct> TYPE zess_0023_t_efct.
***
***DELETE FROM zess_0023_t_efct.
***
***APPEND INITIAL LINE TO lt_efct ASSIGNING <ls_efct>.
***<ls_efct>-efctid = '01'.
***<ls_efct>-efct_text = 'Внедрение в практику работы гражданского служащего новых знаний с целью повышения качества профессиональной деятельности'.
***APPEND INITIAL LINE TO lt_efct ASSIGNING <ls_efct>.
***<ls_efct>-efctid = '02'.
***<ls_efct>-efct_text = 'Обеспечение уровня профессиональных знаний, необходимых для исполнения гражданским служащим должностных обязанностей'.
***APPEND INITIAL LINE TO lt_efct ASSIGNING <ls_efct>.
***<ls_efct>-efctid = '03'.
***<ls_efct>-efct_text = 'Включение гражданского служащего на конкурсной основе в кадровый резерв'.
***APPEND INITIAL LINE TO lt_efct ASSIGNING <ls_efct>.
***<ls_efct>-efctid = '04'.
***<ls_efct>-efct_text = 'Назначение гражданского служащего на иную должность гражданской службы на конкурсной основе в порядке должностного роста'.
***APPEND INITIAL LINE TO lt_efct ASSIGNING <ls_efct>.
***<ls_efct>-efctid = '05'.
***<ls_efct>-efct_text = 'Присвоение гражданскому служащему первого или очередного классного чина'.
***APPEND INITIAL LINE TO lt_efct ASSIGNING <ls_efct>.
***<ls_efct>-efctid = '06'.
***<ls_efct>-efct_text = 'Освоение новых профессиональных знаний и умений для успешного прохождения аттестации'.
***APPEND INITIAL LINE TO lt_efct ASSIGNING <ls_efct>.
***<ls_efct>-efctid = '07'.
***<ls_efct>-efct_text = 'Освоение новых научных знаний в целях продолжения замещения должности гражданской службы и качественного исполнения должностных обязанностей'.
***APPEND INITIAL LINE TO lt_efct ASSIGNING <ls_efct>.
***<ls_efct>-efctid = '08'.
***<ls_efct>-efct_text = 'Другая'.
***
***MODIFY zess_0023_t_efct FROM TABLE  lt_efct.

*============== Справочник "Цели профессионального образования" ===============
*Код заполнения закоментарен, чтобы не затереть ничего
*                       ВНИМАНИЕ!!!!
*РАСКОММЕНТИРОВАНИЕ И ЗАПУСК МОЖЕТ УНИТОЖИТЬ ВАЖНЫЕ ДАННЫЕ
*                  ВЫЙДИ БЕЗ СОХРАНЕНИЯ
*DATA:
*  lt_goal TYPE TABLE OF zess_0023_t_goal.
*
*FIELD-SYMBOLS: <ls_goal> TYPE zess_0023_t_goal.
*
*DELETE FROM zess_0023_t_goal.
*
*APPEND INITIAL LINE TO lt_goal ASSIGNING <ls_goal>.
*<ls_goal>-goalid = '01'.
*<ls_goal>-subty = '92'.
*<ls_goal>-goal_text = 'Совершенствование знаний гражданского служащего или получение им дополнительных знаний для выполнения нового вида профессиональной деятельности'.
*
*APPEND INITIAL LINE TO lt_goal ASSIGNING <ls_goal>.
*<ls_goal>-goalid = '02'.
*<ls_goal>-subty = '92'.
*<ls_goal>-goal_text = 'Получение дополнительной квалификации'.
*
*APPEND INITIAL LINE TO lt_goal ASSIGNING <ls_goal>.
*<ls_goal>-goalid = '03'.
*<ls_goal>-subty = '91'.
*<ls_goal>-goal_text = 'Освоение актуальных изменений в конкретных вопросах профессиональной деятельности гражданского служащего'.
*
*APPEND INITIAL LINE TO lt_goal ASSIGNING <ls_goal>.
*<ls_goal>-goalid = '04'.
*<ls_goal>-subty = '91'.
*<ls_goal>-goal_text = 'Комплексное обновление знаний гражданского служащего по ряду вопросов в установленной сфере профессиональной служебной деятельности для решения соответствующих профессиональных задач'.
*
*MODIFY zess_0023_t_goal FROM TABLE  lt_goal.

*==============Справочник "Виды дополнительного образования"=============
DATA:
  lt_aedu TYPE TABLE OF zess_0023_t_aedu.

FIELD-SYMBOLS: <ls_aedu> TYPE zess_0023_t_aedu.

DELETE FROM zess_0023_t_aedu.

APPEND INITIAL LINE TO lt_aedu ASSIGNING <ls_aedu>.
<ls_aedu>-addeduid = '91'.
<ls_aedu>-addedu_text = 'Повышение квалификации'.

APPEND INITIAL LINE TO lt_aedu ASSIGNING <ls_aedu>.
<ls_aedu>-addeduid = '92'.
<ls_aedu>-addedu_text = 'Профессиональная переподготовка'.

MODIFY zess_0023_t_aedu FROM TABLE  lt_aedu.
