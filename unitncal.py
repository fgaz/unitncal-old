#!/usr/bin/env python
# -*- coding: utf-8 -*-

import urllib2, json, datetime, re, os, time

import sys;
reload(sys);
sys.setdefaultencoding("utf8")

        # Centro Interdipartimentale Biologia Integrata- CIBio
codes = { "Laurea - Scienze e Tecnologie Biomolecolari - 0516G": 10116
        , "Laurea Magistrale - BIOTECNOLOGIE CELLULARI E MOLECOLARI - 0520H": 10232
        , "Laurea Magistrale - Biologia Quantitativa e Computazionale - 0521H": 10616
        # Centro interdipartimentale Mente/Cervello- CIMeC
        , "Laurea Magistrale - Cognitive Science - Scienze Cognitive - 0708H": 10168
        # Dipartimento di Economia e Management
        , "Laurea - Amministrazione Aziendale e Diritto - 0115G": 10126
        , "Laurea - Gestione Aziendale - 0116G": 10128
        , "Laurea - Economia e Management - 0117G": 10129
        , "Laurea Magistrale - International Management - Management Internazionale - 0119H": 10136
        , "Laurea Magistrale - Innovation Management - Management dell'innovazione - 0120H": 10176
        , "Laurea Magistrale - Economics - Economia - 0121H": 10178
        , "Laurea Magistrale - Finanza - 0122H": 10179
        , "Laurea Magistrale - Management - 0123H": 10185
        , "Laurea Magistrale - Management - 0124H": 10186
        , "Laurea Magistrale - Economia e legislazione d'impresa - 0125H": 10187
        , "Laurea Magistrale - Management della sostenibilità e del turismo - 0126H": 10557
        # Dipartimento di Fisica
        , "Laurea - Fisica - 0513G": 10113
        , "Laurea Magistrale - FISICA - 0518H": 10169
        # Dipartimento di Ingegneria Civile, Ambientale e Meccanica
        , "Laurea - Ingegneria Civile - 0325G": 10127
        , "Laurea - Ingegneria per l'ambiente e il territorio - 0326G": 10130
        , "Laurea Magistrale - Ingegneria Civile - 0331H": 10149
        , "Laurea Magistrale - Ingegneria per l'ambiente e il territorio - 0332H": 10150
        , "Laurea Magistrale - Ingegneria Energetica - 0337H": 10437
        , "Laurea Magistrale Ciclo Unico 5 anni - Ingegneria Edile-Architettura - 0336F": 10175
        # Dipartimento di Ingegneria e Scienza dell'Informazione
        , "Laurea - Informatica - 0514G": 10114
        , "Laurea - Ingegneria Elettronica e delle Telecomunicazioni - 0329G": 10133
        , "Laurea - Ingegneria dell'informazione e Organizzazione D'impresa - 0330G": 10134
        , "Laurea - Ingegneria dell'Informazione e delle Comunicazioni - 0338G": 10560
        , "Laurea Magistrale - Ingegneria dell’Informazione e delle Comunicazioni - 0340H": 10627
        , "Laurea Magistrale - Ingegneria delle telecomunicazioni - 0335H": 10153
        , "Laurea Magistrale - INFORMATICA - 0517H": 10117
        # Dipartimento di Ingegneria Industriale
        , "Laurea - Ingegneria Industriale - 0327G": 10131
        , "Laurea Magistrale - Ingegneria Meccatronica - 0333H": 10151
        , "Laurea Magistrale - Materials and production Engineering - Ingegneria dei materiali e della produzione - 0339H": 10563
        # Dipartimento di Lettere e Filosofia
        , "Laurea - Filosofia - 0416G": 10155
        , "Laurea - Beni culturali - 0417G": 10156
        , "Laurea - Studi storici e filologico-letterari - 0419G": 10158
        , "Laurea - Lingue moderne - 0427G": 10438
        , "Laurea Magistrale - Letterature euroamericane, traduzione e critica letteraria - 0422H": 10164
        , "Laurea Magistrale - Mediazione linguistica, turismo e culture - 0423H": 10165
        , "Laurea Magistrale - Filologia e critica letteraria - 0424H": 10166
        , "Laurea Magistrale - Filosofia e linguaggi della modernità - 0420H": 10167
        , "Laurea Magistrale - Scienze storiche - 0426H": 10233
        # Dipartimento di Matematica
        , "Laurea - Matematica - 0515G": 10115
        , "Laurea Magistrale - MATEMATICA - 0519H": 10170
        # Dipartimento di Psicologia e Scienze Cognitive
        , "Laurea - Interfacce e Tecnologie della Comunicazione - 0704G": 10112
        , "Laurea - Scienze e Tecniche di Psicologia Cognitiva - 0705G": 10123
        , "Laurea Magistrale - Psicologia - 0707H": 10124
        , "Laurea Magistrale - Human-Computer Interaction - Interazione Persona-Macchina - 0709H": 10559
        # Dipartimento di Sociologia e Ricerca Sociale
        , "Master di Secondo Livello - Previsione Sociale - M218": 10507
        , "Laurea - Studi internazionali - 0620G": 10565
        , "Laurea - Servizio sociale - 0622G": 10624
        , "Laurea - Sociologia - 0611G": 10137
        , "Laurea - Studi internazionali - 0612G": 10138
        , "Laurea - Servizio Sociale - 0613G": 10139
        , "Laurea Magistrale - Gestione delle organizzazioni e del territorio - 0618H": 10234
        , "Laurea Magistrale - Metodologia, Organizzazione e Valutazione dei Servizi Sociali - 0619H": 10235
        , "Laurea Magistrale - Sociology and social research - Sociologia e ricerca sociale - 0621H": 10567
        # Scuola di studi Internazionali
        , "Laurea Magistrale - EUROPEAN AND INTERNATIONAL STUDIES - STUDI EUROPEI E INTERNAZIONALI  - 0803H": 10177
        , "Laurea Magistrale - Studi sulla Sicurezza Internazionale - 0804H": 10615 }
years = [1,2,3,4,5]
cooldown = 1 #seconds

def converttime(unix):
  return datetime.datetime.utcfromtimestamp(int(unix)).strftime("%Y%m%dT%H%M%SZ")

def parselocation(subj, desc):
  if subj in [10114, 10133, 10134, 10560, 10627, 10153, 10117]:
    return parselocationDISI(desc)
  m = re.search('\[(?P<roomid>Aula [^\]]*)\]', desc, re.I | re.U)
  if not m:
    return "Posizione sconosciuta"
  cod = m.group('roomid')
  if not cod:
    return "Posizione sconosciuta"
  return cod

def parselocationDISI(desc):
  m = re.search('\[Aula ((pc )| )?(?P<roomid>[AB]?\d\d\d[^\]]*)\]', desc, re.I | re.U)
  if not m:
    return "Posizione sconosciuta"
  cod = m.group('roomid')
  if not cod:
    return "Posizione sconosciuta"
  building = "Edificio sconosciuto"
  floor = "Piano sconosciuto"
  room = "Aula " + cod
  if cod and cod[0]=="A":
    building = "Povo 1"
    floor = "Piano " + cod[1]
  if cod and cod[0]=="B":
    building = "Povo 2"
    floor = "Piano " + cod[1]
  if cod and cod[0] in [str(x) for x in range(0,10)]:
    building = "Povo 0"
    floor = "Piano " + cod[0]
  return building + ", " + floor + ", " + room

def makecal(subj, year):
  cal = ""

  j = urllib2.urlopen("https://webapps.unitn.it/Orari/it/Web/AjaxEventi/c/"
                      + str(subj)
                      + "-"
                      + str(year)
                      + "/?start=0&end=2000000000").read()
  
  obj=json.loads(j)
  
  events = obj["Eventi"]
  
  summaries = {}
  
  for summary in obj["Attivita"]:
    summaries[summary["IdADfisica"]] = summary["DescrizioneAD"]
  
  cal+="BEGIN:VCALENDAR\n"
  
  for event in events:
    cal+= "BEGIN:VEVENT\n"
    summary = summaries[int(event["id"])]
    description = json.dumps(event["title"]) # hacky
    start = converttime(event["start"])
    end = converttime(event["end"])
    location = parselocation(subj, description)
    cal+= "DTSTART:" + start + "\n"
    cal+= "DTEND:" + end + "\n"
    cal+= "SUMMARY:" + summary + "\n"
    cal+= "DESCRIPTION:" + description + "\n"
    cal+= "LOCATION:" + location + "\n"
    cal+= "END:VEVENT\n"
  
  cal+= "END:VCALENDAR\n"
  return cal

def main():
  basedir = "cal/"
  if 'OPENSHIFT_REPO_DIR' in os.environ: # if we are using openshift
    basedir = os.environ['OPENSHIFT_REPO_DIR'] + "cal/"
  if not os.path.exists(basedir):
    os.makedirs(basedir)
  for subj in codes.values():
    for year in years:
      cal = makecal(subj, year)
      f = open(basedir + str(subj) + "-" + str(year),"w")
      f.write(cal)
      f.close()
      time.sleep(cooldown)

if __name__ == "__main__":
  main()
