# Python 3
import logging
import pymssql
import random

from HearingAidBDO import HearingAidBDO


def generate_ha_shapes():
    shapes = ["rite", "mini-bte", "bte", "ric", "cic", "ite", "itc"]
    conn = pymssql.connect(server='MED-PC037', user='sa', password='19Zeljo21', database='ohl_data')
    cursor = conn.cursor()
    cursor.execute("SELECT COUNT(*) FROM OHLData")
    max_ohl_id = cursor.fetchone()
    max_ohl_id = max_ohl_id[0]
    ohl_id = 1
    while ohl_id <= max_ohl_id:
        i = int(round(random.gauss(3.5, 1)))
        if i > 6:
            i = 6
        if i < 0:
            i = 0
        cursor.execute("UPDATE OHLData SET AidShape=\'{0}\' WHERE OHLDataID={1} AND AidShape IS NULL".format(shapes[i], str(ohl_id)))
        logging.info("generate_ha_shapes(): " + str(ohl_id) + " rows updated.\n")
        ohl_id = ohl_id + 1
    conn.commit()
    conn.close()
    return


def generate_algorithm_types():
    types = ["linear", "NAL-NL1", "NAL-NL2", "DSP5", "proprietary"]
    conn = pymssql.connect(server='MED-PC037', user='sa', password='19Zeljo21', database='ohl_data')
    cursor = conn.cursor()
    cursor.execute("SELECT COUNT(*) FROM OHLData")
    max_ohl_id = cursor.fetchone()
    max_ohl_id = max_ohl_id[0]
    ohl_id = 1
    while ohl_id <= max_ohl_id:
        i = int(round(random.gauss(len(types) / 2, 1)))
        if i > len(types) - 1:
            i = len(types) - 1
        if i < 0:
            i = 0
        cursor.execute("UPDATE OHLData SET FittingFormula=\'{0}\' WHERE OHLDataID={1} AND FittingFormula IS NULL".format(types[i], str(ohl_id)))
        logging.info("generate_algorithm_types(): " + str(ohl_id) + " rows updated.\n")
        ohl_id = ohl_id + 1
    conn.commit()
    conn.close()
    return


def generate_numbers_of_channels():
    channels = [8, 12, 16, 20]
    conn = pymssql.connect(server='MED-PC037', user='sa', password='19Zeljo21', database='ohl_data')
    cursor = conn.cursor()
    cursor.execute("SELECT COUNT(*) FROM OHLData")
    max_ohl_id = cursor.fetchone()
    max_ohl_id = max_ohl_id[0]
    ohl_id = 1
    while ohl_id <= max_ohl_id:
        i = int(round(random.gauss(len(channels) / 2, 1)))
        if i > len(channels) - 1:
            i = len(channels) - 1
        if i < 0:
            i = 0
        cursor.execute("UPDATE OHLData SET NumberOfChannels=\'{0}\' WHERE OHLDataID={1} AND NumberOfChannels IS NULL".format(channels[i], str(ohl_id)))
        logging.info("generate_numbers_of_channels(): " + str(ohl_id) + " rows updated.\n")
        ohl_id = ohl_id + 1
    conn.commit()
    conn.close()
    return


def generate_aid_names():
    # brands = ["Phonak", "Unitron", "Oticon", "Widex", "Starkey", "Sonova",
    # "Signia", "Siemens", "Resound", "Beltone"]
    # names = ["Nera", "Alta", "Opn", "Naida Q", "Audeo", "Virto", "Bolero",
    # "Pure", "Carat", "Celion", "Motion", "Moxi", "Stride", "Max", "Flex"]

    ha_data = HearingAidBDO()
    no_of_brands = len(ha_data.brands)
    conn = pymssql.connect(server='MED-PC037', user='sa', password='19Zeljo21', database='ohl_data')
    cursor = conn.cursor()
    cursor.execute("SELECT COUNT(*) FROM OHLData")
    max_ohl_id = cursor.fetchone()
    max_ohl_id = max_ohl_id[0]
    ohl_id = 1
    while ohl_id <= max_ohl_id:
        brand_index = int(round(random.gauss(no_of_brands / 2, 1)))
        if brand_index > no_of_brands - 1:
            brand_index = no_of_brands - 1
        if brand_index < 0:
            brand_index = 0
        no_of_names = len(ha_data.names[brand_index])
        name_index = int(round(random.gauss(no_of_names / 2, 1)))
        if name_index > no_of_names - 1:
            name_index = no_of_names - 1
        if name_index < 0:
            name_index = 0
        cursor.execute("UPDATE OHLData SET Brand=\'{0}\', AidName=\'{1}\' WHERE OHLDataID={2}".format(ha_data.brands[brand_index], ha_data.names[brand_index][name_index], str(ohl_id)))
        logging.info("generate_aid_names(): " + str(ohl_id) + " rows updated.\n")
        ohl_id = ohl_id + 1
    conn.commit()
    conn.close()
    return


logging.basicConfig(level=logging.INFO)
# generate_ha_shapes()
# generate_algorithm_types()
# generate_numbers_of_channels()
generate_aid_names()
