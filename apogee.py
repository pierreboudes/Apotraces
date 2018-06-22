#!/usr/bin/env python

# On passe par un tunnel SSH à vous d'en déterminer les paramètres et de le lancer avant
# d'exécuter le script
# ssh -L 1521:192.168.0.…:1521 me@ourgateway.fr
localport = '1521'
print("Ce script tente d'accéder à la base de donnée Apogée via le port "+ localport)
print('si cela échoue essayez de faire un tunnel comme dans cet exemple :')
print('ssh -L ' + localport + ':IPAPOGEE:1521 you@yourgateway.fr')
#
# On a besoin des libs Oracle
# http://cx-oracle.readthedocs.io/en/latest/installation.html#quick-start-cx-oracle-installation
# on peut les installer en local, unzip puis cd oracle/instantclient_12_1/

import cx_Oracle
import hashlib
from secret import *

####file secret.py ######
# login = ''            #
# password = ''         #
# secretsalt = ''       #
#########################

quote=''  #quote = '"' for quotes
sep = '|' # CSV separator
outfilename = 'data.csv'

con = cx_Oracle.connect(login + '/' + password + '@127.0.0.1:' + localport + '/PRO')

requete = """
SELECT individu.cod_etu as cod_etu,
      Adresse_fixe.COD_BDI AS code_postal,
      TYP_DIPLOME.LIC_TPD AS lib_diplome,
      LPAD (
         DECODE (
        VDI_FRACTIONNER_VET.COD_SIS_DAA_MIN,
        NULL, '01',
        DECODE (
           DECODE (
              VDI_FRACTIONNER_VET.COD_SIS_DAA_MAX,
              VDI_FRACTIONNER_VET.COD_SIS_DAA_MIN, 0,
              DECODE (GREATEST (INS_ADM_ETP.NBR_INS_ETP, 1),
                  INS_ADM_ETP.NBR_INS_ETP, 1,
                  0)),
           1, DECODE (
             GREATEST (
                  VDI_FRACTIONNER_VET.COD_SIS_DAA_MIN
                + INS_ADM_ETP.NBR_INS_ETP
                - 1,
                VDI_FRACTIONNER_VET.COD_SIS_DAA_MAX),
             VDI_FRACTIONNER_VET.COD_SIS_DAA_MAX, VDI_FRACTIONNER_VET.
                                  COD_SIS_DAA_MIN
                                  + INS_ADM_ETP.
                                NBR_INS_ETP
                                  - 1,
             VDI_FRACTIONNER_VET.COD_SIS_DAA_MAX),
           VDI_FRACTIONNER_VET.COD_SIS_DAA_MIN)),
         2,
         '0')
             AS niveau_dans_le_diplome,
      DIPLOME_SISE.LIB_INT1_DIS AS libelle_discipline_diplome,
      DIPLOME_SISE.COD_DIS AS code_sise_diplome,
      Etape_iae.COD_CYC AS code_cycle,
      Etape_iae.COD_ETP AS code_etape,
      Etape_iae.LIC_ETP AS libelle_court_etape,
      Version_etape_iae.LIB_WEB_VET AS libelle_long_etape,
      COMPOSANTE.LIC_CMP AS libelle_court_composante,
      ACADEMIE.LIC_ACD AS libelle_academie_bac,
      IND_BAC.DAA_OBT_BAC_IBA AS annee_bac,
      ETB_BAC.COD_POS_ADR_ETB AS libelle_code_postal_etb_bac,
      BAC_OUX_EQU.LIC_BAC AS libelle_court_bac,
      DECODE (BAC_OUX_EQU.LIC_BAC,
          'E-mathemat', 'Bacs generaux S',
          'C-mathemat', 'Bacs generaux S',
          'D-math nat', 'Bacs generaux S',
          'E-mathemat', 'Bacs generaux S',
          'S-Sciences', 'Bacs generaux S',
          'A1-science', 'Bacs generaux L',
          'A2-langues', 'Bacs generaux L',
          'A3-art pla', 'Bacs generaux L',
          'A4-langmat', 'Bacs generaux L',
          'A5-langues', 'Bacs generaux L',
          'A6-edu mus', 'Bacs generaux L',
          'A-philo', 'Bacs generaux L',
          'L-litterat', 'Bacs generaux L',
          'L-litterat', 'Bacs generaux L',
          'A7-art pla', 'Bacs generaux L',
          'Bacs generaux L', 'Bacs generaux L',
          'B-eco soc', 'Bacs generaux ES',
          'ES-Eco', 'Bacs generaux ES',
          'B-eco soc', 'Bacs generaux ES',
          'Bacs generaux ES', 'Bacs generaux ES',
          'G1-adminis', 'Bacs technos STMG',
          'G2-gestion', 'Bacs technos STMG',
          'G3-commerc', 'Bacs technos STMG',
          'G-gestion', 'Bacs technos STMG',
          'H-informat', 'Bacs technos STMG',
          'STG-Tech.G', 'Bacs technos STMG',
          'STT-Tech.T', 'Bacs technos STMG',
          'STMG', 'Bacs technos STMG',
          'F4-genie c', 'Bacs technos STI2D',
          'F3-electec', 'Bacs technos STI2D',
          'F2-electro', 'Bacs technos STI2D',
          'F10A-appar', 'Bacs technos STI2D',
          'F10-microt', 'Bacs technos STI2D',
          'F12-art ap', 'Bacs technos STI2D',
          'F1-constru', 'Bacs technos STI2D',
          'F2-electro', 'Bacs technos STI2D',
          'F3-electec', 'Bacs technos STI2D',
          'F4-genie c', 'Bacs technos STI2D',
          'F9-equipem', 'Bacs technos STI2D',
          'F-techniqu', 'Bacs technos STI2D',
          'STI-Tech.I', 'Bacs technos STI2D',
          'STD2A', 'Bacs technos STI2D',
          'STI2D', 'Bacs technos STI2D',
          'F5-physiqu', 'Bacs technos STL',
          'F6-chimie', 'Bacs technos STL',
          'F7-biobio', 'Bacs technos STL',
          'F7-biochim', 'Bacs technos STL',
          'STL-Tech.L', 'Bacs technos STL',
          'F8-med soc', 'Bacs technos ST2S',
          'F8-med soc', 'Bacs technos ST2S',
          'SMS-Med-So', 'Bacs technos ST2S',
          'SMS-Med-So', 'Bacs technos ST2S',
          'ST2S', 'Bacs technos ST2S',
          'F11-musiqu', 'Bacs technos autre',
          'F11P-danse', 'Bacs technos autre',
          'HOT-Hotell', 'Bacs technos autre',
          'HOT-hotell', 'Bacs technos autre',
          'HOT-Hotell', 'Bacs technos autre',
          'STAE-A-Env', 'Bacs technos autre',
          'ST Agro Vi', 'Bacs technos autre',
          '0021-indus', 'Bacs professionnels industriels',
          '0022-terti', 'Bacs professionnels tertiaires',
          '0023-agri', 'Bacs professionnels agricoles',
          '0000-sans', 'Equivalences',
          '0001-intl', 'Equivalences',
          '0002-europ', 'Equivalences',
          '0030-capac', 'Equivalences',
          '0031-etran', 'Equivalences',
          '0032-disp', 'Equivalences',
          '0033-ESEUA', 'Equivalences',
          '0034-ESEUB', 'Equivalences',
          '0035-promo', 'Equivalences',
          '0036-valid', 'Equivalences',
          '0037-autre', 'Equivalences',
          'DAEUA', 'Equivalences',
          'DAEUB', 'Equivalences',
          '0033-DAEUA', 'Equivalences',
          '0034-DAEUB', 'Equivalences',
          '0031-etran', 'Equivalences',
          'D''-agronom', 'Bacs generaux S',
          '')
         AS regroupement_bac,
      TO_NUMBER (TO_CHAR (INDIVIDU.DATE_NAI_IND, 'YYYY'))
         AS annee_naissance,
      REGIME_INS.LIC_RGI AS libelle_regime,
      REGIME_INS.COD_RGI code_regime,
      INS_ADM_ANU.COD_ANU AS annee_inscription
     FROM INS_ADM_ETP,
      INS_ADM_ANU,
      ADRESSE Adresse_fixe,
      IND_BAC,
      INDIVIDU,
      COMMUNE Com_individu,
      --commune com_etb,
      DIPLOME_SISE,
      VDI_FRACTIONNER_VET,
      VERSION_ETAPE Version_etape_iae,
      ETAPE Etape_iae,
      VERSION_DIPLOME,
      DIPLOME Diplom_dispense_etb,
      ETABLISSEMENT Etb_bac,
      PAYS Pays_nationalite,
      BAC_OUX_EQU,
      DEPARTEMENT Dept_bac,
      TYP_DIPLOME,
      COMPOSANTE,
      ACADEMIE,
      REGIME_INS
    WHERE            --COM_ETB.COD_COM=ETB_BAC.COD_COM_ADR_ETB and
      (   INDIVIDU.COD_IND(+) = Adresse_fixe.COD_IND)
      AND (IND_BAC.COD_IND = INDIVIDU.COD_IND)
      AND (INDIVIDU.COD_PAY_NAT = Pays_nationalite.COD_PAY)
      AND (IND_BAC.TEM_INS_ADM = 'O')
      AND (Etb_bac.COD_ETB(+) = IND_BAC.COD_ETB)
      AND (Dept_bac.COD_DEP(+) = IND_BAC.COD_DEP)
      AND (IND_BAC.COD_BAC = BAC_OUX_EQU.COD_BAC)
      AND (Com_individu.COD_COM(+) = Adresse_fixe.COD_COM)
      AND (INS_ADM_ETP.COD_CMP = COMPOSANTE.COD_CMP)
      AND (Diplom_dispense_etb.COD_DIP(+) = INS_ADM_ETP.COD_DIP)
      AND (Etape_iae.COD_ETP = INS_ADM_ETP.COD_ETP)
      AND (INS_ADM_ETP.COD_ETP = Version_etape_iae.COD_ETP
           AND INS_ADM_ETP.COD_VRS_VET = Version_etape_iae.COD_VRS_VET)
      AND (INS_ADM_ETP.COD_DIP = VERSION_DIPLOME.COD_DIP(+)
           AND INS_ADM_ETP.COD_VRS_VDI = VERSION_DIPLOME.COD_VRS_VDI(+))
      AND (INS_ADM_ETP.COD_ETP = VDI_FRACTIONNER_VET.COD_ETP(+)
           AND INS_ADM_ETP.COD_VRS_VET =
              VDI_FRACTIONNER_VET.COD_VRS_VET(+)
           AND INS_ADM_ETP.COD_DIP = VDI_FRACTIONNER_VET.COD_DIP(+)
           AND INS_ADM_ETP.COD_VRS_VDI =
              VDI_FRACTIONNER_VET.COD_VRS_VDI(+))
      AND (VERSION_DIPLOME.COD_SIS_VDI = DIPLOME_SISE.COD_DIS(+))
      AND (INDIVIDU.COD_IND = INS_ADM_ANU.COD_IND)
      AND (INS_ADM_ANU.COD_RGI = REGIME_INS.COD_RGI)
      AND (INS_ADM_ANU.COD_ANU = INS_ADM_ETP.COD_ANU
           AND INS_ADM_ANU.COD_IND = INS_ADM_ETP.COD_IND)
      AND (Diplom_dispense_etb.COD_TPD_ETB = TYP_DIPLOME.COD_TPD_ETB)
      AND (Dept_bac.COD_ACD = ACADEMIE.COD_ACD)
      --      AND (IND_BAC.COD_TPE != '10' OR IND_BAC.COD_TPE IS NULL)
      AND (INS_ADM_ANU.COD_ANU IN
          (
           '2017',
           '2016',
           '2015',
           '2014',
           '2013',
           '2012',
           '2011',
           '2010',
           '2009',
           '2008',
           '2007',
           '2048')
           AND DECODE (INS_ADM_ETP.ETA_IAE, 'A', 'O', 'N') IN ('N')
           AND DECODE (
              INS_ADM_ETP.ETA_IAE,
              'E', DECODE (INS_ADM_ETP.ETA_PMT_IAE, 'A', 'N', 'O'),
              'N') IN
              ('O')
           AND INS_ADM_ETP.TEM_IAE_PRM IN ('O'))
"""

cur = con.cursor()
cur.execute(requete)

print('la communication avec Apogée semble fonctionner et la requête tourne')
n = 0
with  open(outfilename,"w",encoding="UTF-8") as outfile:
    for line in cur:
        xs = list(map(str,line))
        ine = xs[0] # on chiffre l'INE
        inec = hashlib.sha224((secretsalt + ine).encode('UTF-8')).hexdigest()
        xs[0] = inec
        outfile.write(quote + (quote + sep + quote).join(xs) + quote + "\n")
        n += 1
print('Terminé ! ', outfilename, ' contient ', n, ' lignes\n')
cur.close()
con.close()
