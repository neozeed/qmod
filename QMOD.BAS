' QMod v-0.23 : November 2002
' Code: Pasco
'
' Additional credits:       
' Thunder - for his oh-so-useful mod file documentation
' Qasir   - It's all his fault I started working on this again after giving up
'           on it so very long ago
'
' * Release notes and whatnot:
'
' This code is provided on an as-is basis, I'm not going to make any sort of
' guarantee that it's going to work properly on your box.  If it does, great,
' if not, feel free to suggest a solution
'
' This player can freely be hacked up and whatnot for whatever purpose, but
' give credit where credit is due 'eh d:
'
'
' * Latest changes
'
' - Was still trying to mix channels with volume set to 0, oops
' - 16 bit mixing is now a bit faster (!) than 8 bit
'
'
' * Issues, issues, and more issues
'
' - Support for pitch changing effects needs to be added
' - Recoding of the mixing engine to perhaps use EMS for sample storage
'   (maybe even part of the page frame for same segment mixing), which should
'   speed things up, as well as allow loading of larger mods
' - Maybe changing the mixing routine from it's half/half buffer filling
'   arrangement to a row by row mix, dropping out cpu stalling with larger
'   buffer sizes, and allowing different tempos
' - There seems to be a playback problem with _tiny_ loops
' - Faking the auto-init is reportedly still a bit flaky on some cards/OSes
' - Some code commenting would probably go down well too (;

DECLARE SUB mod.startloop ()
DECLARE SUB sb.start (freq AS LONG, buflen AS LONG, stereo%, signage%, sbmode%, bits%, inaddr&)
DECLARE SUB mod.play ()
DECLARE FUNCTION sb.dma.remaining& ()
DECLARE SUB sb.dsp.write (byte%)
DECLARE FUNCTION sb.dsp.read% ()
DECLARE FUNCTION sb.init% ()
DECLARE FUNCTION sb.dsp.reset% ()
DECLARE FUNCTION flipword& (in$)
DEFINT A-Z
DIM cpos AS LONG

' Mix volume is in the range 0-255
CONST MIXVOLUME = 64

TYPE PATTERNDATA
   period AS INTEGER
   sample AS INTEGER
   effect AS INTEGER
   effectvalue AS INTEGER
END TYPE

TYPE sample
   segment AS INTEGER
   Offset AS INTEGER
   samplelength AS LONG
   FineTune AS INTEGER
   linearvolume AS INTEGER
   RepeatOffset AS LONG
   RepeatLength AS LONG
   repeatend AS LONG
   samplename AS STRING * 22

END TYPE

TYPE channel
   sample AS INTEGER
   samplepos AS LONG
  
   samplefinepos AS LONG
   samplemixspeed AS LONG
  
   SampleNote AS INTEGER
   SampleNoteFine AS INTEGER

   effect AS INTEGER
   effectvalue AS INTEGER

   period AS INTEGER

   volume AS INTEGER
END TYPE

'CONST timeconst = 7093789
CONST timeconst = 7159090

CONST OUTPUT.MONO = &H0, OUTPUT.STEREO = &H20
CONST OUTPUT.UNSIGNED = &H0, OUTPUT.SIGNED = &H10
CONST OUTPUT.SINGLE = 0, OUTPUT.AUTOINIT = &H6
CONST OUTPUT.8BIT = &H10, OUTPUT.16BIT = 0

CONST dmacmask = &HA, dmacmode = &HB, dmacff = &HC
CONST hdmacmask = &HD4, hdmacmode = &HD6, hdmacff = &HD8

DIM SHARED mixrate AS LONG, bufferlen AS LONG, bitrate AS INTEGER, stereo AS INTEGER
DIM SHARED bufferticks, mixlen

DIM SHARED baseport, dma, hdma, irq

DIM SHARED dmaaddr, dmacount, dmapage
DIM SHARED hdmaaddr, hdmacount, hdmapage
DIM SHARED ainitack, ccountport

DIM SHARED title AS STRING
DIM SHARED samples(31) AS sample
DIM SHARED numchannels, numsamples
DIM SHARED numpatterns, songendjump, songlength

DIM SHARED speed, rowticks
DIM SHARED order, row, nexthalf

DIM SHARED samplepos AS LONG, samplefinepos AS LONG

nexthalf = 1

'$DYNAMIC
   DIM SHARED patterntable(127)
'$STATIC

DIM SHARED volmixes(255, 64)

DIM SHARED channels(4) AS channel

CLS

PRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄ ÄÄÄÄ ÄÄÄ ÄÄ Ä  Ä   Ä"
PRINT "³"
PRINT "³   QModPlay, version -0.23                                      ³"
PRINT "³                                                                 "
PRINT "³    Usage: modplay options filespec                             ³"
PRINT "                                                                 ³"
PRINT "³  Options: -S    stereo playback                                 "
PRINT "³           -Bxx  specify bitrate (8 or 16)        - Default 16  ³"
PRINT "            -Fxx  specify frequency (6/8/11/22/44) - Default 22  ³"
PRINT "³           -Txx  number of ticks to prebuffer     - Default 2   ³"
PRINT "                                                                 ³"
PRINT "               Ä   Ä  Ä Ä ÄÄ ÄÄÄ ÄÄÄÄ ÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
PRINT

IF sb.init = -1 THEN
   PRINT "*** SB INIT ERROR"
   END
END IF

'DEF SEG = VARSEG(mixarea(0))
DEF SEG = VARSEG(mixarea)

' Load mod

mixrate = 22050
bitrate = 16
bufferticks = 4
mixlen = 2

IF INSTR(COMMAND$, "-B") THEN
   SELECT CASE VAL(MID$(COMMAND$, INSTR(COMMAND$, "-B") + 2, 2))
      CASE 8: bitrate = 8
      CASE 16: bitrate = 16
      CASE ELSE: PRINT "Bad bitrate! Using 8-bit output"
   END SELECT
END IF

IF INSTR(COMMAND$, "-F") THEN
   f = VAL(MID$(COMMAND$, INSTR(COMMAND$, "-F") + 2, 2))
   SELECT CASE VAL(MID$(COMMAND$, INSTR(COMMAND$, "-F") + 2, 2))
      CASE 6: mixrate = 6250
      CASE 8: mixrate = 8350
      CASE 11: mixrate = 11050
      CASE 22: mixrate = 22050
      CASE 44: mixrate = 44100
      CASE ELSE: PRINT "Bad frequency! Using 22kHz output"
   END SELECT
END IF

IF INSTR(COMMAND$, "-T") THEN
   mixlen = VAL(MID$(COMMAND$, INSTR(COMMAND$, "-T") + 2, 3))
   IF mixlen < 0 THEN mixlen = 2
   bufferticks = mixlen * 2
END IF

IF INSTR(COMMAND$, "-S") THEN
   stereo = 1
END IF

bufferlen = mixrate / 50 * bufferticks

IF bitrate = 8 THEN
   '$DYNAMIC
      pbufferlen = (bufferlen + 1) \ 2
   '$STATIC
ELSE
   '$DYNAMIC
      pbufferlen = (bufferlen + 1)
   '$STATIC
END IF
    
IF stereo THEN
   pbufferlen = pbufferlen * 2
END IF

IF stereo = 0 THEN
   FOR vol = 0 TO 64
      mixlevel& = vol * MIXVOLUME
      FOR samp = 0 TO 255
         IF samp AND &H80 THEN
            volmixes(samp, vol) = (samp OR &HFF00) * mixlevel& \ 64
         ELSE
            volmixes(samp, vol) = samp * mixlevel& \ 64
         END IF
      NEXT
   NEXT
ELSE
   FOR vol = 0 TO 64
      mixlevel& = vol * MIXVOLUME
      FOR samp = 0 TO 255
         IF samp AND &H80 THEN
            volmixes(samp, vol) = (samp OR &HFF00) * mixlevel& \ 32
         ELSE
            volmixes(samp, vol) = samp * mixlevel& \ 32
         END IF
      NEXT
   NEXT
END IF

mbufferlen = bufferlen
IF stereo THEN mbufferlen = mbufferlen * 2

'$DYNAMIC
   DIM SHARED playbuffer(pbufferlen)
'$STATIC

DIM SHARED postmix(mbufferlen)


cmd$ = RTRIM$(COMMAND$)

WHILE INSTR(cmd$, " ")
   cmd$ = MID$(cmd$, INSTR(cmd$, " ") + 1)
WEND

OPEN cmd$ FOR INPUT AS #1
CLOSE #1

OPEN cmd$ FOR BINARY ACCESS READ AS #1

PRINT "Loading mod   : " + cmd$
PRINT "Checking type : ";

SEEK #1, 1081
mtype$ = INPUT$(4, 1)
SELECT CASE mtype$
   CASE "M.K.", "M!K!": mtype$ = "Amiga 4-chan MOD": numchannels = 4: numsamples = 31
   CASE "FLT4": mtype$ = "Startrekker 4-chan MOD": numchannels = 4: numsamples = 31
   CASE "4CHN": mtype$ = "FastTracker 4-chan MOD": numchannels = 4: numsamples = 31
   CASE ELSE: PRINT mtype$: mtype$ = "Error!"
END SELECT

PRINT mtype$
IF mtype$ = "Error!" THEN PRINT "Unable to determine mod type or type not supported": END

SEEK #1, 1
title$ = INPUT$(20, 1)
total& = 0
PRINT " Loading sample headers"
FOR l = 1 TO numsamples
   samples(l).samplename = INPUT$(22, 1)
   samples(l).samplelength = flipword(INPUT$(2, 1)) * 2
   samples(l).FineTune = ASC(INPUT$(1, 1))
   samples(l).linearvolume = ASC(INPUT$(1, 1))
   samples(l).RepeatOffset = flipword(INPUT$(2, 1)) * 2
   samples(l).RepeatLength = flipword(INPUT$(2, 1)) * 2
   samples(l).repeatend = samples(l).RepeatOffset + samples(l).RepeatLength
   IF samples(l).RepeatLength < 4 THEN samples(l).repeatend = &H7FFFFFF
  
   total& = total& + samples(l).samplelength
NEXT

remaining& = LOF(1) - 154 - numsamples * 30 - total&
IF remaining& / 1024 <> remaining& \ 1024 THEN
   PRINT "Mod be broked!"
   END
END IF

numpatterns = remaining& \ 1024

songlength = ASC(INPUT$(1, 1))
songendjump = ASC(INPUT$(1, 1))
IF songendjump >= 127 THEN songendjump = 0

FOR l = 0 TO 127
   patterntable(l) = ASC(INPUT$(1, 1))
NEXT

PRINT " Loading patterns ("; numpatterns; ")"

'$DYNAMIC
DIM SHARED patterns(numpatterns, 3, 63) AS PATTERNDATA
'$STATIC

SEEK #1, 1085

FOR l = 0 TO numpatterns - 1
      FOR row = 0 TO 63
   FOR chan = 0 TO numchannels - 1
         byte1 = ASC(INPUT$(1, 1))
         byte2 = ASC(INPUT$(1, 1))
         byte3 = ASC(INPUT$(1, 1))
         byte4 = ASC(INPUT$(1, 1))
         patterns(l, chan, row).sample = (byte1 AND &HF0) + (byte3 \ 16)
         patterns(l, chan, row).period = byte2 + ((byte1 AND 15) * 256)
         patterns(l, chan, row).effect = byte3 AND 15
         patterns(l, chan, row).effectvalue = byte4
      NEXT
   NEXT
NEXT


samplea = (total& + 63) \ 64

'$DYNAMIC
   DIM SHARED samplespace(samplea, 31) AS INTEGER
'$STATIC

baseseg& = VARSEG(samplespace(0, 0))
DEF SEG = baseseg&
abspos& = 0

cseg& = baseseg&
cpos = 0

PRINT " Loading sample 0";
FOR l = 1 TO numsamples
   LOCATE , 1
   PRINT " Loading sample"; l;

   cseg& = baseseg& + abspos& \ 16
   cpos = abspos& AND 15

   samples(l).segment = VAL("&h" + HEX$(cseg&))
   samples(l).Offset = abspos& AND 15

   DEF SEG = samples(l).segment

   FOR lp = -32767 TO -32768 + samples(l).samplelength
      x = ASC(INPUT$(1, 1))
      POKE cpos, x
      cpos = cpos + 1
      abspos& = abspos& + 1
   NEXT
   
NEXT

CLOSE #1

row = -1
speed = 6

LOCATE 14, 1
PRINT title$; STRING$(50, 32)
PRINT STRING$(78, 32)
PRINT "Playing at"; RTRIM$(STR$(mixrate)); "Hz,"; RTRIM$(STR$(bitrate)); "-bit ";
IF stereo THEN PRINT "stereo ";  ELSE PRINT "mono ";
PRINT "("; LTRIM$(STR$(mixlen)); " tick buffer)"
PRINT STRING$(78, 32)
COLOR 4

PRINT STRING$(28, 196); "ÒÄÄÄÄÄÄÄÒÄÄÄÄÄÄÄÒ"; STRING$(34, 196); "·"

PRINT : PRINT : PRINT : PRINT

PRINT STRING$(28, 196); "ÐÄÄÄÄÄÄÄÐÄÄÄÄÄÄÄÐ"; STRING$(34, 196); "½"

PRINT STRING$(78, 32);

mod.startloop

DO
   mod.play
LOOP UNTIL INKEY$ <> ""
null = sb.dsp.reset

END

FUNCTION flipword& (in$)
tmp$ = MID$(in$, 2, 1)
tmp$ = tmp$ + LEFT$(in$, 1) + CHR$(0) + CHR$(0)

flipword& = CVL(tmp$)

END FUNCTION

SUB mod.play STATIC

   DIM totals(3)  AS LONG

   oremains& = remains&
   remains& = sb.dma.remaining

   dohalf = 0

   IF nexthalf THEN
      IF remains& > bufferlen \ 2 THEN dohalf = 1
   ELSE
      IF remains& < bufferlen \ 2 THEN dohalf = 1
      null = INP(ainitack)
   END IF

   IF dohalf THEN
     
      starttime& = remains&

      mixpos = 0

      IF bitrate = 16 THEN mixpos = nexthalf * bufferlen \ 2

      mixpertick = mixrate \ 50
     
      DEF SEG = VARSEG(postmix(0))

      IF stereo THEN
         FOR l2 = mixpos * 4 TO (mixpos + mixlen * mixpertick) * 4 - 1
            POKE l2, 0
         NEXT
      ELSE
         FOR l2 = mixpos * 2 TO (mixpos + mixlen * mixpertick) * 2 - 1
            POKE l2, 0
         NEXT
      END IF

      FOR l = 1 TO mixlen
        
         IF rowticks <= 0 THEN
           
            row = row + 1
           
            FOR n = 0 TO 3
               effval = channels(n).effectvalue
               SELECT CASE channels(n).effect
                  CASE &HD:
                     row = (effval AND 15) + (effval \ 16) * 10
                     order = order + 1
                     IF order >= songlength THEN order = songendjump
               END SELECT
            NEXT

            IF row >= 64 THEN
               row = 0
               order = order + 1
               IF order = songlength THEN
                  order = songendjump
               END IF
            END IF

            currentpattern = patterntable(order)

            FOR n = 0 TO 3

               channels(n).effect = patterns(currentpattern, n, row).effect
               channels(n).effectvalue = patterns(currentpattern, n, row).effectvalue
              
               IF patterns(currentpattern, n, row).period THEN
                  channels(n).period = patterns(currentpattern, n, row).period
                  channels(n).samplepos = 0
                  channels(n).samplefinepos = 0
                  freq& = timeconst / (channels(n).period * 2)
                  channels(n).samplemixspeed = freq& / mixrate * 65536

               END IF
              
               IF patterns(currentpattern, n, row).sample THEN
                 
                  IF channels(n).sample = 0 THEN
                     channels(n).samplepos = 0
                     channels(n).samplefinepos = 0
                  END IF

                  channels(n).sample = patterns(currentpattern, n, row).sample
                  channels(n).volume = samples(channels(n).sample).linearvolume
               END IF
              
            NEXT
           
            FOR n = 0 TO 3
               effval = channels(n).effectvalue
               SELECT CASE channels(n).effect
                  CASE 9: channels(n).samplepos = effval * 256&
                 
                  CASE &HC: IF effval > 64 THEN effval = 64
                     channels(n).volume = effval
                 
                  CASE &HE:
                     SELECT CASE effval \ 16
                        CASE &HA:
                           channels(n).volume = channels(n).volume + effval \ 16
                           IF channels(n).volume > 64 THEN channels(n).volume = 64
                        CASE &HB:
                           channels(n).volume = channels(n).volume - effval \ 16
                           IF channels(n).volume < 0 THEN channels(n).volume = 0
                     END SELECT
                 
                  CASE &HF: IF effval <= 31 AND effval > 0 THEN speed = effval

               END SELECT
            NEXT

            rowticks = speed

         END IF

         rowticks = rowticks - 1
       
         FOR n = 0 TO 3
            effval = channels(n).effectvalue
            SELECT CASE channels(n).effect
               CASE &HA:
                  IF effval AND &HF0 THEN
                     IF rowticks < speed - 1 THEN
                        channels(n).volume = channels(n).volume + effval \ 16
                        IF channels(n).volume > 64 THEN channels(n).volume = 64
                     END IF
                  ELSE
                     IF rowticks < speed - 1 THEN
                        channels(n).volume = channels(n).volume - (effval AND 15)
                        IF channels(n).volume < 0 THEN channels(n).volume = 0
                     END IF
                  END IF
            END SELECT
         NEXT
        
         FOR n = 0 TO 3
            totals(n) = 0
            IF channels(n).sample AND channels(n).volume <> 0 THEN
               DEF SEG = samples(channels(n).sample).segment
               inoff = samples(channels(n).sample).Offset
               cmixpos = mixpos
               
               volm = channels(n).volume
              
               samplepos& = channels(n).samplepos + inoff
               samplefinepos& = channels(n).samplefinepos
               samplemixspeed& = channels(n).samplemixspeed
               samplelength& = samples(channels(n).sample).samplelength
               repeatend& = samples(channels(n).sample).repeatend

               sampend& = samplelength& + inoff
               samptype = 0
               IF repeatend& <> &H7FFFFFF THEN
                  samptype = 1
                  sampend& = repeatend& + inoff
               END IF

               IF stereo = 0 THEN
                  t& = totals(n)
                  FOR l2 = 0 TO mixpertick - 1
                     nv = volmixes(PEEK(samplepos&), volm)

                     postmix(cmixpos) = postmix(cmixpos) + nv
                     cmixpos = cmixpos + 1
                    
                     t& = t& + ABS(nv)
                    
                     samplefinepos& = samplefinepos& + samplemixspeed&
                   
                     samplepos& = samplepos& + samplefinepos& \ 65536
                     samplefinepos& = samplefinepos& AND 65535
                   
                     IF samplepos& > sampend& THEN
                        IF samptype = 1 THEN
                           DO
                              samplepos& = samplepos& + samples(channels(n).sample).RepeatOffset - sampend&
                           LOOP UNTIL samplepos& <= sampend&
                        ELSE
                           channels(n).sample = 0
                           EXIT FOR
                        END IF
                     END IF
                  NEXT
                  totals(n) = t&
               ELSE
                  off1 = 1: off2 = 0
                  IF n = 0 OR n = 3 THEN off1 = 0: off2 = 1
                 
                  cmixpos = cmixpos * 2 + off1

                  t& = totals(n)
                    
                  IF samptype <> 1 THEN
                     samplefinpos& = samplepos& + (samplefinepos& + samplemixspeed& * mixpertick) \ 65536
                    
                     tickmix = mixpertick
                    
                     IF samplefinpos& >= sampend& THEN
                        tickmix = 0
                        IF samplepos& < sampend& THEN
                           tickmix = ((sampend& - samplepos&) * 65536 - samplefinepos&) \ samplemixspeed&
                        END IF
                        channels(n).sample = 0
                     END IF
                     
                     FOR l2 = 1 TO tickmix
                        nv = volmixes(PEEK(samplepos&), volm)
                        postmix(cmixpos) = postmix(cmixpos) + nv
                        cmixpos = cmixpos + 2
                        t& = t& + ABS(nv)
                        samplefinepos& = samplefinepos& + samplemixspeed&
                        samplepos& = samplepos& + samplefinepos& \ 65536
                        samplefinepos& = samplefinepos& AND 65535
                     NEXT
                     
                  ELSE
                    
                     FOR l2 = 1 TO mixpertick
                        nv = volmixes(PEEK(samplepos&), volm)
                        postmix(cmixpos) = postmix(cmixpos) + nv
                        cmixpos = cmixpos + 2
                        t& = t& + ABS(nv)
                        samplefinepos& = samplefinepos& + samplemixspeed&
                        samplepos& = samplepos& + samplefinepos& \ 65536
                        samplefinepos& = samplefinepos& AND 65535
                        IF samplepos& > sampend& THEN
                           DO
                              samplepos& = samplepos& + samples(channels(n).sample).RepeatOffset - sampend&
                           LOOP UNTIL samplepos& <= sampend&
                        END IF
                     NEXT
                  END IF
                  totals(n) = t&
               END IF

               channels(n).samplepos = samplepos& - inoff
               channels(n).samplefinepos = samplefinepos&

            END IF
            
         NEXT

         mixpos = mixpos + mixpertick
         
      NEXT
     
     
      IF bitrate = 8 THEN
         ps = 0
        
         IF stereo = 0 THEN
            DEF SEG = VARSEG(playbuffer(nexthalf * bufferlen \ 4))
            ofs = VARPTR(playbuffer(nexthalf * bufferlen \ 4))
           
            FOR l2 = 1 TO mixpertick * mixlen
               POKE ofs, postmix(ps) \ 256
               ofs = ofs + 1
               ps = ps + 1
            NEXT
         ELSE
            DEF SEG = VARSEG(playbuffer(nexthalf * bufferlen \ 2))
            ofs = VARPTR(playbuffer(nexthalf * bufferlen \ 2))
          
            FOR l2 = 1 TO mixpertick * mixlen
               POKE ofs, postmix(ps) \ 256
               POKE ofs + 1, postmix(ps + 1) \ 256
               ofs = ofs + 2
               ps = ps + 2
            NEXT
         END IF
      END IF

      nexthalf = 1 - nexthalf

      endtime& = sb.dma.remaining
      elapsed = starttime& - endtime&
     
      tbufferlen = bufferlen \ 2
      IF stereo THEN tbufferlen = bufferlen

      IF elapsed < 0 THEN elapsed = elapsed - tbufferlen
     
      LOCATE , 56
      'COLOR 7, 0
      cpu = (cpu + 200& * elapsed / tbufferlen) / 2
      PRINT USING "CPU:###.#%"; cpu;
     EXIT SUB
     
      LOCATE 19, 1
      FOR n = 0 TO 3
         COLOR 9: PRINT "[  ] ";
         
         IF channels(n).sample <> 0 THEN
            LOCATE , 2
            COLOR 11
            IF channels(n).sample < 10 THEN PRINT "0";
            PRINT LTRIM$(STR$(channels(n).sample));
            LOCATE , 6
           
            COLOR 10
            IF channels(n).volume = 0 THEN COLOR 2
            PRINT samples(channels(n).sample).samplename;
            COLOR 4: PRINT " º";
            COLOR 3
            LOCATE , 30
            mn = 1
            FOR l = 1 TO 4
               mn = mn * 10
               IF channels(n).samplepos < mn THEN PRINT " ";
            NEXT
            PRINT channels(n).samplepos;
            COLOR 4: PRINT "º";
            
            COLOR 3
            freq& = channels(n).samplemixspeed * mixrate \ 65536
            mn = 1
            FOR l = 1 TO 4
               mn = mn * 10
               IF freq& < mn THEN PRINT " ";
            NEXT
           
            PRINT freq&;
            COLOR 4: PRINT "º ";
            COLOR 4
           
            fill = 32 * totals(n) / (mixpertick * mixlen) / 4096
            IF fill > 32 THEN fill = 32
           
            COLOR 11
            PRINT STRING$(fill, "ð");
            COLOR 1
            PRINT STRING$(32 - fill, "ð");
            COLOR 4
            PRINT " º";
            
         ELSE
            
            LOCATE , 2
            COLOR 3
            PRINT "--";
            LOCATE , 5
            COLOR 4
            PRINT STRING$(23, 32); " º       º       º ";
            COLOR 1
            PRINT STRING$(32, "ð");
            
         END IF
         PRINT
      NEXT

   END IF

'   IF sb.dma.remaining = 0 THEN mod.startloop
'   LOCATE 1, 1: PRINT sb.dma.remaining
END SUB

SUB mod.startloop

   IF bitrate = 8 THEN
      IF stereo THEN
         sb.start mixrate, bufferlen * 2, OUTPUT.STEREO, OUTPUT.SIGNED, OUTPUT.AUTOINIT, OUTPUT.8BIT, VAL("&h" + HEX$(VARSEG(playbuffer(0))) + "&") * 16& + VARPTR(playbuffer(0))
      ELSE
         sb.start mixrate, bufferlen, OUTPUT.MONO, OUTPUT.SIGNED, OUTPUT.AUTOINIT, OUTPUT.8BIT, VAL("&h" + HEX$(VARSEG(playbuffer(0))) + "&") * 16& + VARPTR(playbuffer(0))
      END IF
   ELSE
      IF stereo THEN
         sb.start mixrate, bufferlen * 2, OUTPUT.STEREO, OUTPUT.SIGNED, OUTPUT.AUTOINIT, OUTPUT.16BIT, VAL("&h" + HEX$(VARSEG(postmix(0))) + "&") * 16& + VARPTR(postmix(0))
      ELSE
         sb.start mixrate, bufferlen, OUTPUT.MONO, OUTPUT.SIGNED, OUTPUT.AUTOINIT, OUTPUT.16BIT, VAL("&h" + HEX$(VARSEG(postmix(0))) + "&") * 16& + VARPTR(postmix(0))
      END IF
   END IF

END SUB

FUNCTION sb.dma.remaining&

OUT dmacff, 255

'buflen& = INP(ccountport)
'buflen& = buflen& + INP(ccountport) * 256

'sb.dma.remaining& = buflen&
sb.dma.remaining& = INP(ccountport) + 256& * INP(ccountport)
END FUNCTION

FUNCTION sb.dsp.read

   WHILE INP(baseport + 14) AND 128: WEND
   sb.dsp.read = INP(baseport + 10)

END FUNCTION

FUNCTION sb.dsp.reset

   OUT baseport + 6, 1
   OUT baseport + 6, 0
  
   t! = TIMER: DO
   v = INP(baseport + 14)
   LOOP UNTIL TIMER - t! > .2 OR (v AND 128)
   IF v AND 128 = 0 THEN sb.resetdsp = -1: EXIT FUNCTION

   IF INP(baseport + 10) <> &HAA THEN sb.dsp.reset = -1: EXIT FUNCTION


END FUNCTION

SUB sb.dsp.write (byte)

   WHILE INP(baseport + 12) AND 128: WEND
   OUT baseport + 12, byte

END SUB

FUNCTION sb.init

initstr$ = ENVIRON$("BLASTER")

baseport = VAL("&h" + MID$(initstr$, INSTR(initstr$, "A") + 1, 3))
irq = VAL("&h" + MID$(initstr$, INSTR(initstr$, "I") + 1, 1))
dma = VAL("&h" + MID$(initstr$, INSTR(initstr$, "D") + 1, 1))
IF INSTR(initstr$, "H") THEN hdma = VAL("&h" + MID$(initstr$, INSTR(initstr$, "H") + 1, 1))

PRINT "- SB init"
PRINT "     IRQ :"; irq
PRINT "DSP Port : "; HEX$(baseport); "h"
PRINT " Low DMA :"; dma
PRINT "High DMA :"; : IF hdma THEN PRINT hdma ELSE PRINT " Not Present!"

PRINT

IF sb.dsp.reset = -1 THEN sb.init = -1: EXIT FUNCTION

SELECT CASE dma
   CASE 0: dmaaddr = &H0: dmacount = &H1: dmapage = &H87
   CASE 1: dmaaddr = &H2: dmacount = &H3: dmapage = &H83
   CASE 2: dmaaddr = &H4: dmacount = &H5: dmapage = &H81
   CASE 3: dmaaddr = &H6: dmacount = &H7: dmapage = &H82
   CASE ELSE: PRINT "Bad DMA channel!": sb.init = -1: EXIT FUNCTION
END SELECT

IF hdma THEN
   SELECT CASE hdma
      CASE 4: hdmaaddr = &HC0: hdmacount = &HC2: hdmapage = &H8F
      CASE 5: hdmaaddr = &HC4: hdmacount = &HC6: hdmapage = &H8B
      CASE 6: hdmaaddr = &HC8: hdmacount = &HCA: hdmapage = &H89
      CASE 7: hdmaaddr = &HCC: hdmacount = &HCE: hdmapage = &H8A
      CASE ELSE: PRINT "Bad HDMA channel!": sb.init = -1: EXIT FUNCTION
   END SELECT
END IF

END FUNCTION

SUB sb.start (freq AS LONG, buflen AS LONG, stereo, signage, sbmode, bits, addr&)

IF bits THEN
   sdma = dma
   sdmaaddr = dmaaddr
   sdmacount = dmacount
   sdmapage = dmapage

   sdmacmask = dmacmask
   sdmacmode = dmacmode
   sdmacff = dmacff

ELSE
   sdma = hdma
   sdmaaddr = hdmaaddr
   sdmacount = hdmacount
   sdmapage = hdmapage

   sdmacmask = hdmacmask
   sdmacmode = hdmacmode
   sdmacff = hdmacff
END IF

OUT sdmacmask, 4 + (sdma AND 3)
OUT sdmacff, 0

sb.dsp.write &H41
sb.dsp.write freq& \ 256
sb.dsp.write freq& AND 255

IF sbmode THEN
   OUT sdmacmode, &H58 + (sdma AND 3)
ELSE
   OUT sdmacmode, &H48 + (sdma AND 3)
END IF

linearaddr& = addr&

IF bits THEN
   OUT sdmaaddr, linearaddr& AND 255
   OUT sdmaaddr, linearaddr& \ 256
ELSE
   OUT sdmaaddr, (linearaddr& \ 2) AND 255
   OUT sdmaaddr, linearaddr& \ 512
END IF

OUT sdmacount, (buflen - 1) AND 255
OUT sdmacount, (buflen - 1) \ 256

OUT sdmapage, linearaddr& \ 65536

OUT sdmacmask, sdma AND 3

sb.dsp.write &HB0 + sbmode + bits
sb.dsp.write stereo + signage

sb.dsp.write 255 AND (buflen - 1)
sb.dsp.write ((buflen - 1) \ 256)

IF sbmode THEN
   ainitack = baseport + 15
   IF bits THEN
      ainitack = baseport + 14
   END IF
END IF

ccountport = sdmacount
   

END SUB
