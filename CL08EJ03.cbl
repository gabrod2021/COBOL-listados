      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CL08EJ02.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 WS-VAR-AUXILIARES.
              05 WS-NOMBRE            PIC X(20) OCCURS 10 TIMES.
              05 WS-APELLIDO          PIC X(20) OCCURS 10 TIMES.
              05 WS-DNI               PIC 99.999.999 OCCURS 10 TIMES.
              05 WS-ANO-NACIMIENTO    PIC 9(04) OCCURS 10 TIMES.
              05 WS-EDAD              PIC 9(03) OCCURS 10 TIMES.
              05 WS-MAYOR             PIC 99.999.999 VALUE ZERO.
              05 WS-MAY               PIC 9(03) VALUE ZERO.
           01 WS-INGRESOS.
              02 WS-USUARIO             PIC X(01) VALUE SPACE.
                 88  WS-USUARIO-SI                VALUE 'S'.
                 88  WS-USUARIO-NO                VALUE 'N'.


           01 WS-TITULOS.
              05 WS-TIT-LINEA-1       PIC X(66).
              05 WS-TITULO.
                 10 FILLER               PIC X(03) VALUE SPACES.
                 10 WS-TIT-NOMBRE    PIC X(20) VALUE
                                                'Nombre              '.
                  10 FILLER           PIC X(03) VALUE SPACES.
                  10 WS-TIT-APELLIDO  PIC X(20) VALUE
                                                'Apellido            '.
                  10 FILLER           PIC X(03) VALUE SPACES.
                  10 WS-TIT-DNI       PIC X(10) VALUE 'DNI       '.
                  10 FILLER           PIC X(03) VALUE SPACES.
                  10 WS-TIT-EDAD      PIC X(04) VALUE 'EDAD'.
                  10 FILLER           PIC X(03) VALUE SPACES.


           01 WS-FILA.
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-NOMBRE   PIC X(20).
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-APELLIDO PIC X(20).
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-DNI      PIC X(10).
                  05 FILLER           PIC X(03) VALUE SPACES.
                  05 WS-FILA-EDAD     PIC Z99.
                  05 FILLER           PIC X(03) VALUE SPACES.

           77 WS-FECHA                PIC X(6).
           77 WS-FECHA-AA             PIC 99.
           77 WS-FECHA-AAAA           PIC 9999.

           77 WS-INDICE               PIC 99 .
           77 WS-CANT-USU             PIC 99 .

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.

           PERFORM 0000-INICIAR-PROGRAMA
              THRU 0000-INICIAR-PROGRAMA-EXIT.

           PERFORM 2000-PROCESAR-PROGRAMA
              THRU 2000-PROCESAR-PROGRAMA-EXIT WITH TEST BEFORE
             UNTIL WS-INDICE > 10.

           PERFORM 3000-FINALIZAR-PROGRAMA
              THRU 3000-FINALIZAR-PROGRAMA-EXIT.

           STOP RUN.
      *----------------------------------------------------------------*
       0000-INICIAR-PROGRAMA.

           INITIALIZE WS-VAR-AUXILIARES.

           MOVE ALL '+-'    TO WS-TIT-LINEA-1.

           MOVE 1           TO WS-INDICE.


           ACCEPT WS-FECHA FROM DATE.
           MOVE WS-FECHA(1:2) TO WS-FECHA-AA.
           COMPUTE WS-FECHA-AAAA = 2000 + WS-FECHA-AA.

       0000-INICIAR-PROGRAMA-EXIT.

           EXIT.
      *----------------------------------------------------------------*
       2000-PROCESAR-PROGRAMA.


           DISPLAY "Ingresa nombre de usuario " WS-INDICE ":".
               ACCEPT WS-NOMBRE(WS-INDICE).

           DISPLAY "Ingresa Apellido de usuario " WS-INDICE ":".
               ACCEPT WS-APELLIDO(WS-INDICE).

           DISPLAY "Ingresa DNI de usuario " WS-INDICE ":".
               ACCEPT WS-DNI(WS-INDICE).

           DISPLAY "Ingresa Año de nacimiento de usuario "WS-INDICE ":".
               ACCEPT WS-ANO-NACIMIENTO(WS-INDICE).


                COMPUTE WS-EDAD(WS-INDICE) =
                       WS-FECHA-AAAA - WS-ANO-NACIMIENTO(WS-INDICE).

                IF WS-EDAD(WS-INDICE) > WS-MAY THEN
                    MOVE  WS-EDAD(WS-INDICE) TO WS-MAY
                    MOVE WS-DNI(WS-INDICE) TO WS-MAYOR.



                    ADD 1                      TO WS-INDICE.
                    ADD 1                      TO WS-CANT-USU.




           DISPLAY "Quiere ingresar otro usuario?(S/N)"
           IF WS-CANT-USU EQUAL 10 THEN
            ADD 1 TO WS-CANT-USU
             PERFORM 3000-FINALIZAR-PROGRAMA
             THRU 3000-FINALIZAR-PROGRAMA-EXIT
             STOP RUN
           ELSE
             ACCEPT WS-USUARIO
           END-IF.
           IF WS-USUARIO EQUAL 'N' OR 'n' THEN
             ADD 1 TO WS-CANT-USU
             PERFORM 3000-FINALIZAR-PROGRAMA
             THRU 3000-FINALIZAR-PROGRAMA-EXIT
             STOP RUN.



           DISPLAY WS-TIT-LINEA-1.

           2000-PROCESAR-PROGRAMA-EXIT.

           EXIT.

      *----------------------------------------------------------------*
       3000-FINALIZAR-PROGRAMA.


           DISPLAY WS-TIT-LINEA-1

           DISPLAY WS-TITULO

           DISPLAY WS-TIT-LINEA-1



           MOVE 1                            TO WS-INDICE.



                 PERFORM 3100-MOSTAR-FILA-DATOS
                 THRU 3100-MOSTAR-FILA-DATOS-EXIT
                 UNTIL WS-INDICE > WS-CANT-USU - 1.


           SUBTRACT 1 FROM WS-CANT-USU.

           DISPLAY "Total de usuarios ingresados: " WS-CANT-USU .
           DISPLAY "La mayor edad ingresada es: " WS-MAY .
           DISPLAY "y corresponde al dni: " WS-MAYOR.

       3000-FINALIZAR-PROGRAMA-EXIT.
           EXIT.
      *----------------------------------------------------------------*
       3100-MOSTAR-FILA-DATOS.

           MOVE WS-NOMBRE(WS-INDICE)   TO WS-FILA-NOMBRE.
           MOVE WS-APELLIDO(WS-INDICE) TO WS-FILA-APELLIDO.
           MOVE WS-DNI(WS-INDICE)      TO WS-FILA-DNI.
           MOVE WS-EDAD(WS-INDICE)     TO WS-FILA-EDAD.

           DISPLAY WS-FILA.

            ADD 1                       TO WS-INDICE.

       3100-MOSTAR-FILA-DATOS-EXIT.

           EXIT.
      *----------------------------------------------------------------*

       END PROGRAM CL08EJ02.
