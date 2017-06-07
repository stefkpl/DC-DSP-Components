
    (*********************************************************************
     *  MatroskaSplitter.pas                                             *
     *                                                                   *
     *  This unit is Part of the DC-DSP Audio Filter v1.0                *
     *                                                                   *
     *  author    : Milenko Mitrovic                                     *
     *  email     : dcoder@dsp-worx.de                                   *
     *  web       : http://dsp-worx.de                                   *
     *  date      : 24-07-2003                                           *
     *                                                                   *
     *  The contents of this file are used with permission, subject to   *
     *  the Mozilla Public License Version 1.1 (the "License"); you may  *
     *  not use this file except in compliance with the License. You may *
     *  obtain a copy of the License at                                  *
     *  http://www.mozilla.org/MPL/MPL-1.1.html                          *
     *                                                                   *
     *  Software distributed under the License is distributed on an      *
     *  "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   *
     *  implied. See the License for the specific language governing     *
     *  rights and limitations under the License.                        *
     *                                                                   *
     *  (C) 2003, 2004 Milenko Mitrovic <dcoder@dsp-worx.de>             *
     *                                                                   *
     *********************************************************************)

unit MatroskaSplitter;

interface

uses
  Windows, ActiveX, DirectShow9, BaseClass, SysUtils;

const
  CLSID_MatroskaSplitter : TGUID = '{149D2E01-C32E-4939-80F6-C07B81015A7A}';
  CLSID_MatroskaSource   : TGUID = '{0A68C3B5-9164-4a54-AFAF-995B2FF0E0D4}';
  IID_ITrackInfo         : TGUID = '{03E98D51-DDE7-43aa-B70C-42EF84A3A23D}';
  IID_IChapterInfo       : TGUID = '{8E128709-3DC8-4e49-B632-380FCF496B6D}';
  IID_IKeyFrameInfo      : TGUID = '{01A5BBD3-FE71-487C-A2EC-F585918A8724}';

type
  TTrackType = (
    TypeVideo     = 1,
    TypeAudio     = 2,
    TypeComplex   = 3,
    TypeLogo      = $10,
    TypeSubtitle  = $11,
    TypeControl   = $20
  );

  TTrackElement = packed record
    Size : WORD;                    // Size of this structure
    TrackType : TTrackType;         // See TrackType
    FlagDefault : BOOL;             // Set if the track is the default for its TrackType.
    FlagLacing : BOOL;              // Set if the track may contain blocks using lacing.
    MinCache : DWORD;               // The minimum number of frames a player should be able to cache during playback.
    MaxCache : DWORD;               // The maximum cache size required to store referenced frames in and the current frame. 0 means no cache is needed.
    Language : array[0..3] of Char; // Specifies the language of the track, in the ISO-639-2 form. (end with '\0')
  end;

  TTrackExtendedInfoVideo = packed record
    Size : WORD;            // Size of this structure
    Interlaced : BOOL;      // Set if the video is interlaced.
    PixelWidth : DWORD;     // Width of the encoded video frames in pixels.
    PixelHeight : DWORD;    // Height of the encoded video frames in pixels.
    DisplayWidth : DWORD;   // Width of the video frames to display.
    DisplayHeight : DWORD;  // Height of the video frames to display.
    DisplayUnit : Byte;     // Type of the unit for DisplayWidth/Height (0: pixels, 1: centimeters, 2: inches).
    AspectRatioType : Byte; // Specify the possible modifications to the aspect ratio (0: free resizing, 1: keep aspect ratio, 2: fixed).
  end;

  TTrackExtendedInfoAudio = packed record
    Size : WORD;                      // Size of this structure
    SamplingFreq : Single;            // Sampling frequency in Hz.
    OutputSamplingFrequency : Single; // Real output sampling frequency in Hz (used for SBR techniques).
    Channels : DWORD;                 // Numbers of channels in the track.
    BitDepth : DWORD;                 // Bits per sample, mostly used for PCM.
  end;

  ITrackInfo = interface(IUnknown)
  ['{03E98D51-DDE7-43aa-B70C-42EF84A3A23D}']
    function GetTrackCount : DWORD; stdcall;
    // \param aTrackIdx the track index (from 0 to GetTrackCount()-1)
    function GetTrackInfo(aTrackIdx : DWORD;out pStructureToFill : TTrackElement) : BOOL; stdcall;
    // Get an extended information struct relative to the track type
    function GetTrackExtendedInfo(aTrackIdx : DWORD; out StructureToFill) : BOOL; stdcall;
    function GetTrackCodecID(aTrackIdx : DWORD) : TBStr; stdcall;
    function GetTrackName(aTrackIdx : DWORD) : TBStr; stdcall;
    function GetTrackCodecName(aTrackIdx : DWORD) : TBStr; stdcall;
    function GetTrackCodecInfoURL(aTrackIdx : DWORD) : TBStr; stdcall;
    function GetTrackCodecDownloadURL(aTrackIdx : DWORD) : TBStr; stdcall;
  end;

  TChapterType = (
    AtomicChapter = 0, // only contain one element
    SubChapter    = 1  // contain a list of elements
  );

  TChapterElement = packed record
    Size : WORD;              // size of this structure
    lType : TChapterType;     // see ChapterType
    ChapterId : DWORD;        // unique identifier for this element
    rtStart : TReferenceTime; // REFERENCE_TIME in 100ns
    rtStop : TReferenceTime;  // REFERENCE_TIME in 100ns
  end;

  TChapterElement2 = packed record
    Size : WORD;              // size of this structure
    lType : TChapterType;     // see ChapterType
    ChapterId : DWORD;        // unique identifier for this element
    rtStart : TReferenceTime; // REFERENCE_TIME in 100ns
    rtStop : TReferenceTime;  // REFERENCE_TIME in 100ns
    bDisabled: BOOL;
  end;

const
  CHAPTER_BAD_ID = $FFFFFFFF;
  CHAPTER_ROOT_ID = 0;

type
  IChapterInfo = interface(IUnknown)
  ['{8E128709-3DC8-4e49-B632-380FCF496B6D}']
    // \param aChapterID is 0 for the top level one
    function GetChapterCount(aChapterID : DWORD) : DWORD; stdcall;
    // \param aIndex start from 1 to GetChapterCount( aParentChapterId )
    function GetChapterId(aParentChapterId : DWORD; aIndex : DWORD) : DWORD; stdcall;
    function GetChapterCurrentId : DWORD; stdcall;
    function GetChapterInfo(aChapterID : DWORD; out pStructureToFill) : BOOL; stdcall;
    // \param PreferredLanguage Language code as in ISO-639-2 (3 chars)
    // \param CountryCode       Country code as in internet domains
    function GetChapterStringInfo(aChapterID : DWORD;
      PreferredLanguage: PChar; // 3 char
      CountryCode : PChar       // 2 char
    ) : TBStr; stdcall;
  end;

  PReferenceTimeArray = ^TReferenceTimeArray;
  TReferenceTimeArray = array of TReferenceTime;

  IKeyFrameInfo = interface(IUnknown)
  ['{01A5BBD3-FE71-487C-A2EC-F585918A8724}']
    function GetKeyFrameCount (out nKFs : DWORD) : HRESULT; stdcall; // returns S_FALSE when every frame is a keyframe
    function GetKeyFrames (const pFormat : PGUID; pKFs : PReferenceTimeArray; out nKFs : DWORD{ in, out*}) : HRESULT; stdcall;
  end;

  function Cvt_ISO_639_2_To_Lang(szLang : String) : String;
  function Cvt_ISO_639_2_To_LCID(szLang : String) : DWORD;

implementation

type
  TISO5392LANG = record
    Code : array[0..2] of Char;
    Name : String;
    LCID : DWORD;
  end;

const
  NUM_LANGUAGES = 500;

  ISO5392LANG : array[0..499] of TISO5392LANG = (
    (Code : 'abk'; Name : 'Abkhazian'; LCID : $0),
    (Code : 'ace'; Name : 'Achinese'; LCID : $0),
    (Code : 'ach'; Name : 'Acoli'; LCID : $0),
    (Code : 'ada'; Name : 'Adangme'; LCID : $0),
    (Code : 'aar'; Name : 'Afar'; LCID : $0),
    (Code : 'afh'; Name : 'Afrihili'; LCID : $0),
    (Code : 'afr'; Name : 'Afrikaans'; LCID : $36),
    (Code : 'afa'; Name : 'Afro-Asiatic (Other)'; LCID : $0),
    (Code : 'aka'; Name : 'Akan'; LCID : $0),
    (Code : 'akk'; Name : 'Akkadian'; LCID : $0),
    (Code : 'alb'; Name : 'Albanian'; LCID : $1c),
    (Code : 'sqi'; Name : 'Unknown sqi'; LCID : $0),
    (Code : 'ale'; Name : 'Aleut'; LCID : $0),
    (Code : 'alg'; Name : 'Algonquian languages'; LCID : $0),
    (Code : 'tut'; Name : 'Altaic (Other)'; LCID : $0),
    (Code : 'amh'; Name : 'Amharic'; LCID : $5e),
    (Code : 'apa'; Name : 'Apache languages'; LCID : $0),
    (Code : 'ara'; Name : 'Arabic'; LCID : $01),
    (Code : 'arg'; Name : 'Aragonese'; LCID : $0),
    (Code : 'arc'; Name : 'Aramaic'; LCID : $0),
    (Code : 'arp'; Name : 'Arapaho'; LCID : $0),
    (Code : 'arn'; Name : 'Araucanian'; LCID : $0),
    (Code : 'arw'; Name : 'Arawak'; LCID : $0),
    (Code : 'arm'; Name : 'Armenian'; LCID : $2b),
    (Code : 'hye'; Name : 'Unknown hye'; LCID : $0),
    (Code : 'art'; Name : 'Artificial (Other)'; LCID : $0),
    (Code : 'asm'; Name : 'Assamese'; LCID : $4d),
    (Code : 'ast'; Name : 'Asturian; Bable'; LCID : $0),
    (Code : 'ath'; Name : 'Athapascan languages'; LCID : $0),
    (Code : 'aus'; Name : 'Australian languages'; LCID : $0),
    (Code : 'map'; Name : 'Austronesian (Other)'; LCID : $0),
    (Code : 'ava'; Name : 'Avaric'; LCID : $0),
    (Code : 'ave'; Name : 'Avestan'; LCID : $0),
    (Code : 'awa'; Name : 'Awadhi'; LCID : $0),
    (Code : 'aym'; Name : 'Aymara'; LCID : $0),
    (Code : 'aze'; Name : 'Azerbaijani'; LCID : $2c),
    (Code : 'ast'; Name : 'Bable; Asturian'; LCID : $0),
    (Code : 'ban'; Name : 'Balinese'; LCID : $0),
    (Code : 'bat'; Name : 'Baltic (Other)'; LCID : $0),
    (Code : 'bal'; Name : 'Baluchi'; LCID : $0),
    (Code : 'bam'; Name : 'Bambara'; LCID : $0),
    (Code : 'bai'; Name : 'Bamileke languages'; LCID : $0),
    (Code : 'bad'; Name : 'Banda'; LCID : $0),
    (Code : 'bnt'; Name : 'Bantu (Other)'; LCID : $0),
    (Code : 'bas'; Name : 'Basa'; LCID : $0),
    (Code : 'bak'; Name : 'Bashkir'; LCID : $0),
    (Code : 'baq'; Name : 'Basque'; LCID : $2d),
    (Code : 'eus'; Name : 'Unknown eus'; LCID : $0),
    (Code : 'btk'; Name : 'Batak (Indonesia)'; LCID : $0),
    (Code : 'bej'; Name : 'Beja'; LCID : $0),
    (Code : 'bel'; Name : 'Belarusian'; LCID : $23),
    (Code : 'bem'; Name : 'Bemba'; LCID : $0),
    (Code : 'ben'; Name : 'Bengali'; LCID : $45),
    (Code : 'ber'; Name : 'Berber (Other)'; LCID : $0),
    (Code : 'bho'; Name : 'Bhojpuri'; LCID : $0),
    (Code : 'bih'; Name : 'Bihari'; LCID : $0),
    (Code : 'bik'; Name : 'Bikol'; LCID : $0),
    (Code : 'bin'; Name : 'Bini'; LCID : $0),
    (Code : 'bis'; Name : 'Bislama'; LCID : $0),
    (Code : 'nob'; Name : 'Bokmå'; LCID : $0),
    (Code : 'bos'; Name : 'Bosnian'; LCID : $0),
    (Code : 'bra'; Name : 'Braj'; LCID : $0),
    (Code : 'bre'; Name : 'Breton'; LCID : $0),
    (Code : 'bug'; Name : 'Buginese'; LCID : $0),
    (Code : 'bul'; Name : 'Bulgarian'; LCID : $02),
    (Code : 'bua'; Name : 'Buriat'; LCID : $0),
    (Code : 'bur'; Name : 'Burmese'; LCID : $55),
    (Code : 'mya'; Name : 'Unknown mya'; LCID : $0),
    (Code : 'cad'; Name : 'Caddo'; LCID : $0),
    (Code : 'car'; Name : 'Carib'; LCID : $0),
    (Code : 'spa'; Name : 'Castilian; Spanish'; LCID : $0),
    (Code : 'cat'; Name : 'Catalan'; LCID : $03),
    (Code : 'cau'; Name : 'Caucasian (Other)'; LCID : $0),
    (Code : 'ceb'; Name : 'Cebuano'; LCID : $0),
    (Code : 'cel'; Name : 'Celtic (Other)'; LCID : $0),
    (Code : 'cai'; Name : 'Central American Indian (Other)'; LCID : $0),
    (Code : 'chg'; Name : 'Chagatai'; LCID : $0),
    (Code : 'cmc'; Name : 'Chamic languages'; LCID : $0),
    (Code : 'cha'; Name : 'Chamorro'; LCID : $0),
    (Code : 'che'; Name : 'Chechen'; LCID : $0),
    (Code : 'chr'; Name : 'Cherokee'; LCID : $5c),
    (Code : 'nya'; Name : 'Chewa; Chichewa; Nyanja'; LCID : $0),
    (Code : 'chy'; Name : 'Cheyenne'; LCID : $0),
    (Code : 'chb'; Name : 'Chibcha'; LCID : $0),
    (Code : 'nya'; Name : 'Chichewa; Chewa; Nyanja'; LCID : $0),
    (Code : 'chi'; Name : 'Chinese'; LCID : $04),
    (Code : 'zho'; Name : 'Unknown zho'; LCID : $0),
    (Code : 'chn'; Name : 'Chinook jargon'; LCID : $0),
    (Code : 'chp'; Name : 'Chipewyan'; LCID : $0),
    (Code : 'cho'; Name : 'Choctaw'; LCID : $0),
    (Code : 'zha'; Name : 'Chuang; Zhuang'; LCID : $0),
    (Code : 'chu'; Name : 'Church Slavic; Slavonic; Church Slavonic; Old Bulgarian; Old Church Slavonic'; LCID : $0),
    (Code : 'chu'; Name : 'Church Slavonic; Church Slavic; Old Slavonic; Old Bulgarian; Old Church Slavonic'; LCID : $0),
    (Code : 'chk'; Name : 'Chuukese'; LCID : $0),
    (Code : 'chv'; Name : 'Chuvash'; LCID : $0),
    (Code : 'cop'; Name : 'Coptic'; LCID : $0),
    (Code : 'cor'; Name : 'Cornish'; LCID : $0),
    (Code : 'cos'; Name : 'Corsican'; LCID : $0),
    (Code : 'cre'; Name : 'Cree'; LCID : $0),
    (Code : 'mus'; Name : 'Creek'; LCID : $0),
    (Code : 'crp'; Name : 'Creoles and pidgins (Other)'; LCID : $0),
    (Code : 'cpe'; Name : 'Creoles and pidgin'; LCID : $0),
    (Code : '   '; Name : 'English-based (Other)'; LCID : $0),
    (Code : 'cpf'; Name : 'Creoles and pidgin'; LCID : $0),
    (Code : 'cpp'; Name : 'Creoles and pidgin'; LCID : $0),
    (Code : 'scr'; Name : 'Croatian'; LCID : $1a),
    (Code : 'hrv'; Name : 'Unknown hrv'; LCID : $0),
    (Code : 'cus'; Name : 'Cushitic (Other)'; LCID : $0),
    (Code : 'cze'; Name : 'Czech'; LCID : $05),
    (Code : 'ces'; Name : 'Unknown ces'; LCID : $0),
    (Code : 'dak'; Name : 'Dakota'; LCID : $0),
    (Code : 'dan'; Name : 'Danish'; LCID : $06),
    (Code : 'dar'; Name : 'Dargwa'; LCID : $0),
    (Code : 'day'; Name : 'Dayak'; LCID : $0),
    (Code : 'del'; Name : 'Delaware'; LCID : $0),
    (Code : 'din'; Name : 'Dinka'; LCID : $0),
    (Code : 'div'; Name : 'Divehi'; LCID : $65),
    (Code : 'doi'; Name : 'Dogri'; LCID : $0),
    (Code : 'dgr'; Name : 'Dogrib'; LCID : $0),
    (Code : 'dra'; Name : 'Dravidian (Other)'; LCID : $0),
    (Code : 'dua'; Name : 'Duala'; LCID : $0),
    (Code : 'dut'; Name : 'Dutch; Flemish'; LCID : $13),
    (Code : 'nld'; Name : 'Unknown nld'; LCID : $0),
    (Code : 'dum'; Name : 'Dutc'; LCID : $0),
    (Code : 'dyu'; Name : 'Dyula'; LCID : $0),
    (Code : 'dzo'; Name : 'Dzongkha'; LCID : $0),
    (Code : 'efi'; Name : 'Efik'; LCID : $0),
    (Code : 'egy'; Name : 'Egyptian (Ancient)'; LCID : $0),
    (Code : 'eka'; Name : 'Ekajuk'; LCID : $0),
    (Code : 'elx'; Name : 'Elamite'; LCID : $0),
    (Code : 'eng'; Name : 'English'; LCID : $09),
    (Code : 'enm'; Name : 'Englis'; LCID : $0),
    (Code : 'ang'; Name : 'Englis'; LCID : $0),
    (Code : 'epo'; Name : 'Esperanto'; LCID : $0),
    (Code : 'est'; Name : 'Estonian'; LCID : $25),
    (Code : 'ewe'; Name : 'Ewe'; LCID : $0),
    (Code : 'ewo'; Name : 'Ewondo'; LCID : $0),
    (Code : 'fan'; Name : 'Fang'; LCID : $0),
    (Code : 'fat'; Name : 'Fanti'; LCID : $0),
    (Code : 'fao'; Name : 'Faroese'; LCID : $38),
    (Code : 'fij'; Name : 'Fijian'; LCID : $0),
    (Code : 'fin'; Name : 'Finnish'; LCID : $0b),
    (Code : 'fiu'; Name : 'Finno-Ugrian (Other)'; LCID : $0),
    (Code : 'dut'; Name : 'Flemish; Dutch'; LCID : $13),
    (Code : 'nld'; Name : 'Unknown nld'; LCID : $0),
    (Code : 'fon'; Name : 'Fon'; LCID : $0),
    (Code : 'fre'; Name : 'French'; LCID : $0c),
    (Code : 'fra'; Name : 'Unknown fra'; LCID : $0),
    (Code : 'frm'; Name : 'Frenc'; LCID : $0),
    (Code : 'fro'; Name : 'Frenc'; LCID : $0),
    (Code : 'fry'; Name : 'Frisian'; LCID : $62),
    (Code : 'fur'; Name : 'Friulian'; LCID : $0),
    (Code : 'ful'; Name : 'Fulah'; LCID : $0),
    (Code : 'gaa'; Name : 'Ga'; LCID : $56),
    (Code : 'gla'; Name : 'Gaelic; Scottish Gaelic'; LCID : $3c),
    (Code : 'glg'; Name : 'Gallegan'; LCID : $0),
    (Code : 'lug'; Name : 'Ganda'; LCID : $0),
    (Code : 'gay'; Name : 'Gayo'; LCID : $0),
    (Code : 'gba'; Name : 'Gbaya'; LCID : $0),
    (Code : 'gez'; Name : 'Geez'; LCID : $0),
    (Code : 'geo'; Name : 'Georgian'; LCID : $37),
    (Code : 'kat'; Name : 'Unknown kat'; LCID : $0),
    (Code : 'ger'; Name : 'German'; LCID : $07),
    (Code : 'deu'; Name : 'Unknown deu'; LCID : $0),
    (Code : 'nds'; Name : 'German, Low; Saxon, Low; Low German; Low Saxon'; LCID : $0),
    (Code : 'gmh'; Name : 'Germa'; LCID : $0),
    (Code : 'goh'; Name : 'Germa'; LCID : $0),
    (Code : 'gem'; Name : 'Germanic (Other)'; LCID : $0),
    (Code : 'kik'; Name : 'Gikuyu; Kikuyu'; LCID : $0),
    (Code : 'gil'; Name : 'Gilbertese'; LCID : $0),
    (Code : 'gon'; Name : 'Gondi'; LCID : $0),
    (Code : 'gor'; Name : 'Gorontalo'; LCID : $0),
    (Code : 'got'; Name : 'Gothic'; LCID : $0),
    (Code : 'grb'; Name : 'Grebo'; LCID : $0),
    (Code : 'grc'; Name : 'Greek'; LCID : $08),
    (Code : 'gre'; Name : 'Greek'; LCID : $08),
    (Code : 'ell'; Name : 'Unknown ell'; LCID : $0),
    (Code : 'kal'; Name : 'Greenlandic; Kalaallisut'; LCID : $0),
    (Code : 'grn'; Name : 'Guarani'; LCID : $74),
    (Code : 'guj'; Name : 'Gujarati'; LCID : $47),
    (Code : 'gwi'; Name : 'Gwich´in'; LCID : $0),
    (Code : 'hai'; Name : 'Haida'; LCID : $0),
    (Code : 'hau'; Name : 'Hausa'; LCID : $68),
    (Code : 'haw'; Name : 'Hawaiian'; LCID : $75),
    (Code : 'heb'; Name : 'Hebrew'; LCID : $0d),
    (Code : 'her'; Name : 'Herero'; LCID : $0),
    (Code : 'hil'; Name : 'Hiligaynon'; LCID : $0),
    (Code : 'him'; Name : 'Himachali'; LCID : $0),
    (Code : 'hin'; Name : 'Hindi'; LCID : $39),
    (Code : 'hmo'; Name : 'Hiri Motu'; LCID : $0),
    (Code : 'hit'; Name : 'Hittite'; LCID : $0),
    (Code : 'hmn'; Name : 'Hmong'; LCID : $0),
    (Code : 'hun'; Name : 'Hungarian'; LCID : $0e),
    (Code : 'hup'; Name : 'Hupa'; LCID : $0),
    (Code : 'iba'; Name : 'Iban'; LCID : $0),
    (Code : 'ice'; Name : 'Icelandic'; LCID : $0f),
    (Code : 'isl'; Name : 'Unknown isl'; LCID : $0),
    (Code : 'ido'; Name : 'Ido'; LCID : $0),
    (Code : 'ibo'; Name : 'Igbo'; LCID : $70),
    (Code : 'ijo'; Name : 'Ijo'; LCID : $0),
    (Code : 'ilo'; Name : 'Iloko'; LCID : $0),
    (Code : 'smn'; Name : 'Inari Sami'; LCID : $0),
    (Code : 'inc'; Name : 'Indic (Other)'; LCID : $0),
    (Code : 'ine'; Name : 'Indo-European (Other)'; LCID : $0),
    (Code : 'ind'; Name : 'Indonesian'; LCID : $21),
    (Code : 'inh'; Name : 'Ingush'; LCID : $0),
    (Code : 'ina'; Name : 'Interlingua (International'; LCID : $0),
    (Code : 'ile'; Name : 'Interlingue'; LCID : $0),
    (Code : 'iku'; Name : 'Inuktitut'; LCID : $5d),
    (Code : 'ipk'; Name : 'Inupiaq'; LCID : $0),
    (Code : 'ira'; Name : 'Iranian (Other)'; LCID : $0),
    (Code : 'gle'; Name : 'Irish'; LCID : $3c),
    (Code : 'mga'; Name : 'Iris'; LCID : $0),
    (Code : 'sga'; Name : 'Iris'; LCID : $0),
    (Code : 'iro'; Name : 'Iroquoian languages'; LCID : $0),
    (Code : 'ita'; Name : 'Italian'; LCID : $10),
    (Code : 'jpn'; Name : 'Japanese'; LCID : $11),
    (Code : 'jav'; Name : 'Javanese'; LCID : $11),
    (Code : 'jrb'; Name : 'Judeo-Arabic'; LCID : $0),
    (Code : 'jpr'; Name : 'Judeo-Persian'; LCID : $0),
    (Code : 'kbd'; Name : 'Kabardian'; LCID : $0),
    (Code : 'kab'; Name : 'Kabyle'; LCID : $0),
    (Code : 'kac'; Name : 'Kachin'; LCID : $0),
    (Code : 'kal'; Name : 'Kalaallisut; Greenlandic'; LCID : $0),
    (Code : 'kam'; Name : 'Kamba'; LCID : $0),
    (Code : 'kan'; Name : 'Kannada'; LCID : $4b),
    (Code : 'kau'; Name : 'Kanuri'; LCID : $71),
    (Code : 'kaa'; Name : 'Kara-Kalpak'; LCID : $0),
    (Code : 'kar'; Name : 'Karen'; LCID : $0),
    (Code : 'kas'; Name : 'Kashmiri'; LCID : $60),
    (Code : 'kaw'; Name : 'Kawi'; LCID : $0),
    (Code : 'kaz'; Name : 'Kazakh'; LCID : $3f),
    (Code : 'kha'; Name : 'Khasi'; LCID : $0),
    (Code : 'khm'; Name : 'Khmer'; LCID : $53),
    (Code : 'khi'; Name : 'Khoisan (Other)'; LCID : $0),
    (Code : 'kho'; Name : 'Khotanese'; LCID : $0),
    (Code : 'kik'; Name : 'Kikuyu; Gikuyu'; LCID : $0),
    (Code : 'kmb'; Name : 'Kimbundu'; LCID : $0),
    (Code : 'kin'; Name : 'Kinyarwanda'; LCID : $0),
    (Code : 'kir'; Name : 'Kirghiz'; LCID : $40),
    (Code : 'kom'; Name : 'Komi'; LCID : $0),
    (Code : 'kon'; Name : 'Kongo'; LCID : $0),
    (Code : 'kok'; Name : 'Konkani'; LCID : $57),
    (Code : 'kor'; Name : 'Korean'; LCID : $12),
    (Code : 'kos'; Name : 'Kosraean'; LCID : $0),
    (Code : 'kpe'; Name : 'Kpelle'; LCID : $0),
    (Code : 'kro'; Name : 'Kru'; LCID : $0),
    (Code : 'kua'; Name : 'Kuanyama; Kwanyama'; LCID : $0),
    (Code : 'kum'; Name : 'Kumyk'; LCID : $0),
    (Code : 'kur'; Name : 'Kurdish'; LCID : $0),
    (Code : 'kru'; Name : 'Kurukh'; LCID : $0),
    (Code : 'kut'; Name : 'Kutenai'; LCID : $0),
    (Code : 'kua'; Name : 'Kwanyam'; LCID : $0),
    (Code : 'lad'; Name : 'Ladino'; LCID : $0),
    (Code : 'lah'; Name : 'Lahnda'; LCID : $0),
    (Code : 'lam'; Name : 'Lamba'; LCID : $0),
    (Code : 'lao'; Name : 'Lao'; LCID : $54),
    (Code : 'lat'; Name : 'Latin'; LCID : $76),
    (Code : 'lav'; Name : 'Latvian'; LCID : $26),
    (Code : 'ltz'; Name : 'Letzeburgesch; Luxembourgish'; LCID : $0),
    (Code : 'lez'; Name : 'Lezghian'; LCID : $0),
    (Code : 'lim'; Name : 'Limburgan; Limburger; Limburgish'; LCID : $0),
    (Code : 'lim'; Name : 'Limburger; Limburgan; Limburgish;'; LCID : $0),
    (Code : 'lim'; Name : 'Limburgish; Limburger; Limburgan'; LCID : $0),
    (Code : 'lin'; Name : 'Lingala'; LCID : $0),
    (Code : 'lit'; Name : 'Lithuanian'; LCID : $27),
    (Code : 'loz'; Name : 'Lozi'; LCID : $0),
    (Code : 'lub'; Name : 'Luba-Katanga'; LCID : $0),
    (Code : 'lua'; Name : 'Luba-Lulua'; LCID : $0),
    (Code : 'lui'; Name : 'Luiseno'; LCID : $0),
    (Code : 'smj'; Name : 'Lule Sami'; LCID : $0),
    (Code : 'lun'; Name : 'Lunda'; LCID : $0),
    (Code : 'luo'; Name : 'Luo (Kenya and Tanzania)'; LCID : $0),
    (Code : 'lus'; Name : 'Lushai'; LCID : $0),
    (Code : 'ltz'; Name : 'Luxembourgish; Letzeburgesch'; LCID : $0),
    (Code : 'mac'; Name : 'Macedonian'; LCID : $2f),
    (Code : 'mkd'; Name : 'Unknown mkd'; LCID : $0),
    (Code : 'mad'; Name : 'Madurese'; LCID : $0),
    (Code : 'mag'; Name : 'Magahi'; LCID : $0),
    (Code : 'mai'; Name : 'Maithili'; LCID : $0),
    (Code : 'mak'; Name : 'Makasar'; LCID : $0),
    (Code : 'mlg'; Name : 'Malagasy'; LCID : $0),
    (Code : 'may'; Name : 'Malay'; LCID : $3e),
    (Code : 'msa'; Name : 'Unknown msa'; LCID : $0),
    (Code : 'mal'; Name : 'Malayalam'; LCID : $4c),
    (Code : 'mlt'; Name : 'Maltese'; LCID : $3a),
    (Code : 'mnc'; Name : 'Manchu'; LCID : $0),
    (Code : 'mdr'; Name : 'Mandar'; LCID : $0),
    (Code : 'man'; Name : 'Mandingo'; LCID : $0),
    (Code : 'mni'; Name : 'Manipuri'; LCID : $58),
    (Code : 'mno'; Name : 'Manobo languages'; LCID : $0),
    (Code : 'glv'; Name : 'Manx'; LCID : $0),
    (Code : 'mao'; Name : 'Maori'; LCID : $0),
    (Code : 'mri'; Name : 'Unknown mri'; LCID : $0),
    (Code : 'mar'; Name : 'Marathi'; LCID : $4e),
    (Code : 'chm'; Name : 'Mari'; LCID : $0),
    (Code : 'mah'; Name : 'Marshallese'; LCID : $0),
    (Code : 'mwr'; Name : 'Marwari'; LCID : $0),
    (Code : 'mas'; Name : 'Masai'; LCID : $0),
    (Code : 'myn'; Name : 'Mayan languages'; LCID : $0),
    (Code : 'men'; Name : 'Mende'; LCID : $0),
    (Code : 'mic'; Name : 'Micmac'; LCID : $0),
    (Code : 'min'; Name : 'Minangkabau'; LCID : $0),
    (Code : 'mis'; Name : 'Miscellaneous languages'; LCID : $0),
    (Code : 'moh'; Name : 'Mohawk'; LCID : $0),
    (Code : 'mol'; Name : 'Moldavian'; LCID : $0),
    (Code : 'mkh'; Name : 'Mon-Khmer (Other)'; LCID : $0),
    (Code : 'lol'; Name : 'Mongo'; LCID : $0),
    (Code : 'mon'; Name : 'Mongolian'; LCID : $50),
    (Code : 'mos'; Name : 'Mossi'; LCID : $0),
    (Code : 'mul'; Name : 'Multiple languages'; LCID : $0),
    (Code : 'mun'; Name : 'Munda languages'; LCID : $0),
    (Code : 'nah'; Name : 'Nahuatl'; LCID : $0),
    (Code : 'nau'; Name : 'Nauru'; LCID : $0),
    (Code : 'nav'; Name : 'Navah'; LCID : $0),
    (Code : 'nav'; Name : 'Navajo; Navaho'; LCID : $0),
    (Code : 'nde'; Name : 'Ndebel'; LCID : $0),
    (Code : 'nbl'; Name : 'Ndebel'; LCID : $0),
    (Code : 'ndo'; Name : 'Ndonga'; LCID : $0),
    (Code : 'nap'; Name : 'Neapolitan'; LCID : $0),
    (Code : 'nep'; Name : 'Nepali'; LCID : $61),
    (Code : 'new'; Name : 'Newari'; LCID : $0),
    (Code : 'nia'; Name : 'Nias'; LCID : $0),
    (Code : 'nic'; Name : 'Niger-Kordofanian (Other)'; LCID : $0),
    (Code : 'ssa'; Name : 'Nilo-Saharan (Other)'; LCID : $0),
    (Code : 'niu'; Name : 'Niuean'; LCID : $0),
    (Code : 'non'; Name : 'Nors'; LCID : $0),
    (Code : 'nai'; Name : 'North American Indian (Other)'; LCID : $0),
    (Code : 'sme'; Name : 'Northern Sami'; LCID : $0),
    (Code : 'nde'; Name : 'North Ndebele'; LCID : $0),
    (Code : 'nor'; Name : 'Norwegian'; LCID : $14),
    (Code : 'nob'; Name : 'Norwegian Bokmål; Bokmå'; LCID : $0),
    (Code : 'nno'; Name : 'Norwegian Nynorsk; Nynors'; LCID : $0),
    (Code : 'nub'; Name : 'Nubian languages'; LCID : $0),
    (Code : 'nym'; Name : 'Nyamwezi'; LCID : $0),
    (Code : 'nya'; Name : 'Nyanja; Chichewa; Chewa'; LCID : $0),
    (Code : 'nyn'; Name : 'Nyankole'; LCID : $0),
    (Code : 'nno'; Name : 'Nynors'; LCID : $0),
    (Code : 'nyo'; Name : 'Nyoro'; LCID : $0),
    (Code : 'nzi'; Name : 'Nzima'; LCID : $0),
    (Code : 'oci'; Name : 'Occitan (post 1500); Provençal'; LCID : $0),
    (Code : 'oji'; Name : 'Ojibwa'; LCID : $0),
    (Code : 'chu'; Name : 'Old Bulgarian; Old Slavonic; Church Slavonic; Church Slavic; Old Church Slavonic'; LCID : $0),
    (Code : 'chu'; Name : 'Old Church Slavonic; Old Slavonic; Church Slavonic; Old Bulgarian; Church Slavic'; LCID : $0),
    (Code : 'chu'; Name : 'Old Slavonic; Church Slavonic; Old Bulgarian; Church Slavic; Old Church Slavonic'; LCID : $0),
    (Code : 'ori'; Name : 'Oriya'; LCID : $48),
    (Code : 'orm'; Name : 'Oromo'; LCID : $72),
    (Code : 'osa'; Name : 'Osage'; LCID : $0),
    (Code : 'oss'; Name : 'Ossetian; Ossetic'; LCID : $0),
    (Code : 'oss'; Name : 'Ossetic; Ossetian'; LCID : $0),
    (Code : 'oto'; Name : 'Otomian languages'; LCID : $0),
    (Code : 'pal'; Name : 'Pahlavi'; LCID : $0),
    (Code : 'pau'; Name : 'Palauan'; LCID : $0),
    (Code : 'pli'; Name : 'Pali'; LCID : $0),
    (Code : 'pam'; Name : 'Pampanga'; LCID : $0),
    (Code : 'pag'; Name : 'Pangasinan'; LCID : $0),
    (Code : 'pan'; Name : 'Panjabi'; LCID : $0),
    (Code : 'pap'; Name : 'Papiamento'; LCID : $79),
    (Code : 'paa'; Name : 'Papuan (Other)'; LCID : $0),
    (Code : 'per'; Name : 'Persian'; LCID : $0),
    (Code : 'fas'; Name : 'Unknown fas'; LCID : $0),
    (Code : 'peo'; Name : 'Persia'; LCID : $29),
    (Code : 'phi'; Name : 'Philippine (Other)'; LCID : $0),
    (Code : 'phn'; Name : 'Phoenician'; LCID : $0),
    (Code : 'pon'; Name : 'Pohnpeian'; LCID : $0),
    (Code : 'pol'; Name : 'Polish'; LCID : $15),
    (Code : 'por'; Name : 'Portuguese'; LCID : $16),
    (Code : 'pra'; Name : 'Prakrit languages'; LCID : $0),
    (Code : 'oci'; Name : 'Provençal; Occitan (post 1500)'; LCID : $0),
    (Code : 'pro'; Name : 'Provença'; LCID : $0),
    (Code : 'pus'; Name : 'Pushto'; LCID : $63),
    (Code : 'que'; Name : 'Quechua'; LCID : $0),
    (Code : 'roh'; Name : 'Raeto-Romance'; LCID : $0),
    (Code : 'raj'; Name : 'Rajasthani'; LCID : $0),
    (Code : 'rap'; Name : 'Rapanui'; LCID : $0),
    (Code : 'rar'; Name : 'Rarotongan'; LCID : $0),
    (Code : 'roa'; Name : 'Romance (Other)'; LCID : $0),
    (Code : 'rum'; Name : 'Romanian'; LCID : $18),
    (Code : 'ron'; Name : 'Unknown ron'; LCID : $0),
    (Code : 'rom'; Name : 'Romany'; LCID : $0),
    (Code : 'run'; Name : 'Rundi'; LCID : $0),
    (Code : 'rus'; Name : 'Russian'; LCID : $19),
    (Code : 'sal'; Name : 'Salishan languages'; LCID : $0),
    (Code : 'sam'; Name : 'Samaritan Aramaic'; LCID : $0),
    (Code : 'smi'; Name : 'Sami languages (Other)'; LCID : $3b),
    (Code : 'smo'; Name : 'Samoan'; LCID : $0),
    (Code : 'sad'; Name : 'Sandawe'; LCID : $0),
    (Code : 'sag'; Name : 'Sango'; LCID : $0),
    (Code : 'san'; Name : 'Sanskrit'; LCID : $4f),
    (Code : 'sat'; Name : 'Santali'; LCID : $0),
    (Code : 'srd'; Name : 'Sardinian'; LCID : $0),
    (Code : 'sas'; Name : 'Sasak'; LCID : $0),
    (Code : 'sco'; Name : 'Scots'; LCID : $0),
    (Code : 'gla'; Name : 'Scottish Gaelic; Gaelic'; LCID : $0),
    (Code : 'sel'; Name : 'Selkup'; LCID : $0),
    (Code : 'sem'; Name : 'Semitic (Other)'; LCID : $0),
    (Code : 'scc'; Name : 'Serbian'; LCID : $1a),
    (Code : 'srp'; Name : 'Unknown srp'; LCID : $0),
    (Code : 'srr'; Name : 'Serer'; LCID : $0),
    (Code : 'shn'; Name : 'Shan'; LCID : $0),
    (Code : 'sna'; Name : 'Shona'; LCID : $0),
    (Code : 'iii'; Name : 'Sichuan Yi'; LCID : $0),
    (Code : 'sid'; Name : 'Sidamo'; LCID : $0),
    (Code : 'sgn'; Name : 'Sign languages'; LCID : $0),
    (Code : 'bla'; Name : 'Siksika'; LCID : $0),
    (Code : 'snd'; Name : 'Sindhi'; LCID : $59),
    (Code : 'sin'; Name : 'Sinhalese'; LCID : $5b),
    (Code : 'sit'; Name : 'Sino-Tibetan (Other)'; LCID : $0),
    (Code : 'sio'; Name : 'Siouan languages'; LCID : $0),
    (Code : 'sms'; Name : 'Skolt Sami'; LCID : $0),
    (Code : 'den'; Name : 'Slave (Athapascan)'; LCID : $0),
    (Code : 'sla'; Name : 'Slavic (Other)'; LCID : $0),
    (Code : 'slo'; Name : 'Slovak'; LCID : $1b),
    (Code : 'slk'; Name : 'Unknown slk'; LCID : $0),
    (Code : 'slv'; Name : 'Slovenian'; LCID : $24),
    (Code : 'sog'; Name : 'Sogdian'; LCID : $0),
    (Code : 'som'; Name : 'Somali'; LCID : $77),
    (Code : 'son'; Name : 'Songhai'; LCID : $0),
    (Code : 'snk'; Name : 'Soninke'; LCID : $0),
    (Code : 'wen'; Name : 'Sorbian languages'; LCID : $0),
    (Code : 'nso'; Name : 'Soth'; LCID : $0),
    (Code : 'sot'; Name : 'Soth'; LCID : $0),
    (Code : 'sai'; Name : 'South American Indian (Other)'; LCID : $0),
    (Code : 'sma'; Name : 'Southern Sami'; LCID : $0),
    (Code : 'nbl'; Name : 'South Ndebele'; LCID : $0),
    (Code : 'spa'; Name : 'Spanish; Castilian'; LCID : $0a),
    (Code : 'suk'; Name : 'Sukuma'; LCID : $0),
    (Code : 'sux'; Name : 'Sumerian'; LCID : $0),
    (Code : 'sun'; Name : 'Sundanese'; LCID : $0),
    (Code : 'sus'; Name : 'Susu'; LCID : $0),
    (Code : 'swa'; Name : 'Swahili'; LCID : $41),
    (Code : 'ssw'; Name : 'Swati'; LCID : $0),
    (Code : 'swe'; Name : 'Swedish'; LCID : $1d),
    (Code : 'syr'; Name : 'Syriac'; LCID : $5a),
    (Code : 'tgl'; Name : 'Tagalog'; LCID : $64),
    (Code : 'tah'; Name : 'Tahitian'; LCID : $0),
    (Code : 'tai'; Name : 'Tai (Other)'; LCID : $0),
    (Code : 'tgk'; Name : 'Tajik'; LCID : $28),
    (Code : 'tmh'; Name : 'Tamashek'; LCID : $0),
    (Code : 'tam'; Name : 'Tamil'; LCID : $49),
    (Code : 'tat'; Name : 'Tatar'; LCID : $44),
    (Code : 'tel'; Name : 'Telugu'; LCID : $4a),
    (Code : 'ter'; Name : 'Tereno'; LCID : $0),
    (Code : 'tet'; Name : 'Tetum'; LCID : $0),
    (Code : 'tha'; Name : 'Thai'; LCID : $1e),
    (Code : 'tib'; Name : 'Tibetan'; LCID : $51),
    (Code : 'bod'; Name : 'Unknown bod'; LCID : $0),
    (Code : 'tig'; Name : 'Tigre'; LCID : $0),
    (Code : 'tir'; Name : 'Tigrinya'; LCID : $73),
    (Code : 'tem'; Name : 'Timne'; LCID : $0),
    (Code : 'tiv'; Name : 'Tiv'; LCID : $0),
    (Code : 'tli'; Name : 'Tlingit'; LCID : $0),
    (Code : 'tpi'; Name : 'Tok Pisin'; LCID : $0),
    (Code : 'tkl'; Name : 'Tokelau'; LCID : $0),
    (Code : 'tog'; Name : 'Tonga (Nyasa)'; LCID : $0),
    (Code : 'ton'; Name : 'Tonga (Tonga Islands)'; LCID : $0),
    (Code : 'tsi'; Name : 'Tsimshian'; LCID : $0),
    (Code : 'tso'; Name : 'Tsonga'; LCID : $31),
    (Code : 'tsn'; Name : 'Tswana'; LCID : $0),
    (Code : 'tum'; Name : 'Tumbuka'; LCID : $0),
    (Code : 'tup'; Name : 'Tupi languages'; LCID : $0),
    (Code : 'tur'; Name : 'Turkish'; LCID : $1f),
    (Code : 'ota'; Name : 'Turkis'; LCID : $0),
    (Code : 'tuk'; Name : 'Turkmen'; LCID : $42),
    (Code : 'tvl'; Name : 'Tuvalu'; LCID : $0),
    (Code : 'tyv'; Name : 'Tuvinian'; LCID : $0),
    (Code : 'twi'; Name : 'Twi'; LCID : $0),
    (Code : 'uga'; Name : 'Ugaritic'; LCID : $0),
    (Code : 'uig'; Name : 'Uighur'; LCID : $0),
    (Code : 'ukr'; Name : 'Ukrainian'; LCID : $22),
    (Code : 'umb'; Name : 'Umbundu'; LCID : $0),
    (Code : 'und'; Name : 'Undetermined'; LCID : $0),
    (Code : 'urd'; Name : 'Urdu'; LCID : $20),
    (Code : 'uzb'; Name : 'Uzbek'; LCID : $43),
    (Code : 'vai'; Name : 'Vai'; LCID : $0),
    (Code : 'ven'; Name : 'Venda'; LCID : $33),
    (Code : 'vie'; Name : 'Vietnamese'; LCID : $2a),
    (Code : 'vol'; Name : 'Volapük'; LCID : $0),
    (Code : 'vot'; Name : 'Votic'; LCID : $0),
    (Code : 'wak'; Name : 'Wakashan languages'; LCID : $0),
    (Code : 'wal'; Name : 'Walamo'; LCID : $0),
    (Code : 'wln'; Name : 'Walloon'; LCID : $0),
    (Code : 'war'; Name : 'Waray'; LCID : $0),
    (Code : 'was'; Name : 'Washo'; LCID : $0),
    (Code : 'wel'; Name : 'Welsh'; LCID : $52),
    (Code : 'cym'; Name : 'Unknown cym'; LCID : $0),
    (Code : 'wol'; Name : 'Wolof'; LCID : $0),
    (Code : 'xho'; Name : 'Xhosa'; LCID : $34),
    (Code : 'sah'; Name : 'Yakut'; LCID : $0),
    (Code : 'yao'; Name : 'Yao'; LCID : $0),
    (Code : 'yap'; Name : 'Yapese'; LCID : $0),
    (Code : 'yid'; Name : 'Yiddish'; LCID : $3d),
    (Code : 'yor'; Name : 'Yoruba'; LCID : $6a),
    (Code : 'ypk'; Name : 'Yupik languages'; LCID : $0),
    (Code : 'znd'; Name : 'Zande'; LCID : $0),
    (Code : 'zap'; Name : 'Zapotec'; LCID : $0),
    (Code : 'zen'; Name : 'Zenaga'; LCID : $0),
    (Code : 'zha'; Name : 'Zhuang; Chuang'; LCID : $0),
    (Code : 'zul'; Name : 'Zulu'; LCID : $35),
    (Code : 'zun'; Name : 'Zuni'; LCID : $0)
  );

function Cvt_ISO_639_2_To_Lang(szLang : String) : String;
var
  i : integer;
begin
  Result := 'Unknown (' + szLang + ')';
  for i := 0 to NUM_LANGUAGES -1 do
  begin
    szLang := LowerCase(szLang);
    if szLang = ISO5392LANG[i].Code then
    begin
      Result := ISO5392LANG[i].Name;
      break;
    end;
  end;
end;

function Cvt_ISO_639_2_To_LCID(szLang : String) : DWORD;
var
  i : integer;
begin
  Result := 0;
  for i := 0 to NUM_LANGUAGES -1 do
  begin
    szLang := LowerCase(szLang);
    if szLang = ISO5392LANG[i].Code then
    begin
      Result := ISO5392LANG[i].LCID;
      break;
    end;
  end;
end;

end.
