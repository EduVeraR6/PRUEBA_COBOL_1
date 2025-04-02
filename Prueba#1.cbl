      ******************************************************************
      * Author:Edison Eduardo Vera Romero
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ALUMNOS ASSIGN TO
           'D:\Cobol- Practica\PRUEBA 1\ALUMNOS.DAT'
               ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD ALUMNOS.
       01 ALUMNO-REGISTRO.
           05 ID-ALUMNO         PIC 9(5).
           05 NOMBRE-ALUMNO     PIC X(30).
           05 MATERIAS.
              10 MATERIA OCCURS 5 TIMES PIC X(20).
           05 CALIFICACIONES.
              10 CALIFICACION OCCURS 5 TIMES PIC 9(2).
       WORKING-STORAGE SECTION.
       01 WS-PROMEDIO  PIC 9(5)V99.
       01 WS-SUMA-NOTAS PIC 9(5)V99.
       01 WS-ENCONTRADO PIC X VALUE 'N'.
       01 WS-BUSQUEDA PIC X(30).
       01 WS-STATUS PIC 9(2).
       01 WS-OPCION PIC X.
       01 WS-CONTADOR PIC 9 VALUE ZERO.
       01 WS-BANDERA PIC X VALUE 'N'.
       01 ALUMNO-DATA.
           05 ID-ALUMNO-IN        PIC 9(5).
           05 NOMBRE-ALUMNO-IN    PIC X(30).
           05 MATERIAS-IN.
              10 MATERIA-IN OCCURS 5 TIMES PIC X(20).
           05 CALIFICACIONES-IN.
              10 CALIFICACION-IN OCCURS 5 TIMES PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.


       1000-MENU-OPCIONES.
            DISPLAY "BIENVENIDO AL MENU"
            DISPLAY "1.- REGISTRAR ESTUDIANTE"
            DISPLAY "2.- CALCULAR PROMEDIO"
            DISPLAY "3.- GENERAR REPORTE"
            DISPLAY "4.- SALIR"

            ACCEPT WS-OPCION.

            EVALUATE WS-OPCION
            WHEN 1
                 PERFORM 2000-REGISTRAR-ESTUDIANTE
            WHEN 2
                 PERFORM 3000-CALCULAR-PROMEDIO
            WHEN 3
                 PERFORM 4000-GENERAR-REPORTE
            WHEN 4
                 STOP RUN
            WHEN OTHER
                 DISPLAY "OPCION NO VALIDAD"
                 PERFORM 1000-MENU-OPCIONES
            END-EVALUATE.
            EXIT.


       2000-REGISTRAR-ESTUDIANTE.

           OPEN EXTEND ALUMNOS

           DISPLAY 'Ingrese ID del alumno: '.
           ACCEPT ID-ALUMNO-IN.
           DISPLAY 'Ingrese nombre del alumno: '.
           ACCEPT NOMBRE-ALUMNO-IN.

           PERFORM VARYING WS-CONTADOR FROM 1 BY 1 UNTIL WS-CONTADOR > 5
               DISPLAY 'Ingrese materia ' WS-CONTADOR ': '
               ACCEPT MATERIA-IN(WS-CONTADOR)
               DISPLAY 'Ingrese calificacion ' WS-CONTADOR ': '
               ACCEPT CALIFICACION-IN(WS-CONTADOR)
           END-PERFORM.

           MOVE ID-ALUMNO-IN TO ID-ALUMNO.
           MOVE NOMBRE-ALUMNO-IN TO NOMBRE-ALUMNO.
           MOVE MATERIAS-IN TO MATERIAS.
           MOVE CALIFICACIONES-IN TO CALIFICACIONES.
           WRITE ALUMNO-REGISTRO.
           CLOSE ALUMNOS

           DISPLAY "DESEA REGISTRAR OTRO ESTUDIANTE ?:"
           ACCEPT WS-BANDERA.

           IF WS-BANDERA EQUAL 'N' THEN
                PERFORM 1000-MENU-OPCIONES
           ELSE
                PERFORM 2000-REGISTRAR-ESTUDIANTE
           END-IF.

       3000-CALCULAR-PROMEDIO.
           MOVE 'N' TO WS-BANDERA.

           PERFORM UNTIL WS-BANDERA = 'S'
           DISPLAY "INGRESE EL NOMBRE DEL ESTUDIANTE:"
           ACCEPT WS-BUSQUEDA

           IF FUNCTION LENGTH(WS-BUSQUEDA) > 0 THEN
                 MOVE 'S' TO WS-BANDERA
           ELSE
                 DISPLAY "******************"
                 DISPLAY "BUSQUEDA NO VALIDA"
                 DISPLAY "******************"
           END-IF
           END-PERFORM.

           OPEN INPUT ALUMNOS.

           IF WS-STATUS NOT = "00" AND WS-STATUS NOT = "97"
            DISPLAY "ERROR AL ABRIR EL ARCHIVO. CODIGO: " WS-STATUS
           STOP RUN.

           PERFORM UNTIL WS-STATUS = "10" OR WS-ENCONTRADO = 'S'
             READ ALUMNOS INTO ALUMNO-REGISTRO
               AT END MOVE "10" TO WS-STATUS
             END-READ

            IF NOMBRE-ALUMNO = WS-BUSQUEDA THEN
                 MOVE 'S' TO WS-ENCONTRADO
                 COMPUTE WS-SUMA-NOTAS =
                   CALIFICACION(1) + CALIFICACION(2) + CALIFICACION (3)
                   + CALIFICACION(4) + CALIFICACION(5)
                 COMPUTE WS-PROMEDIO = WS-SUMA-NOTAS / 5
            END-IF
           END-PERFORM.

           CLOSE ALUMNOS.

           IF WS-ENCONTRADO = 'S' THEN
             DISPLAY "ESTUDIANTE: " NOMBRE-ALUMNO
             DISPLAY "PROMEDIO: " WS-PROMEDIO
           ELSE
             DISPLAY "ESTUDIANTE NO ENCONTRADO."

             DISPLAY "DESEA VOLVER AL MENU PRINCIPAL ? S / N "
             ACCEPT WS-OPCION

             EVALUATE WS-OPCION
             WHEN 'S'
                 PERFORM 1000-MENU-OPCIONES
             WHEN 'N'
                 PERFORM 3000-CALCULAR-PROMEDIO
             WHEN OTHER
                 DISPLAY "OPCION NO VALIDA, VOLVIENDO AL MENU PRINCIPAL"
                 PERFORM 1000-MENU-OPCIONES
             END-EVALUATE
           END-IF.
           STOP RUN.

       4000-GENERAR-REPORTE.
            DISPLAY "REPORTE GENERADO"
            STOP RUN.

       END PROGRAM YOUR-PROGRAM-NAME.
