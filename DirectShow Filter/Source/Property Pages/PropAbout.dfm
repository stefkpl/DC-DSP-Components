object FormPropAbout: TFormPropAbout
  Left = 356
  Top = 176
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'About'
  ClientHeight = 439
  ClientWidth = 604
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseMove = Label1MouseMove
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TabControl1: TTabControl
    Left = 8
    Top = 8
    Width = 588
    Height = 423
    TabOrder = 0
    object Label14: TLabel
      Left = 107
      Top = 303
      Width = 3
      Height = 13
    end
    object GroupBox1: TGroupBox
      Left = 16
      Top = 288
      Width = 554
      Height = 117
      Caption = ' License '
      TabOrder = 0
      OnMouseMove = Label1MouseMove
      object Label2: TLabel
        Left = 8
        Top = 15
        Width = 537
        Height = 91
        AutoSize = False
        Caption = 
          'The contents of this file are used with permission, subject to t' +
          'he Mozilla Public License Version 1.1 (the "License"); you may n' +
          'ot use this file except in compliance with the License. You may ' +
          'obtain a copy of the License at http://www.mozilla.org/MPL/MPL-1' +
          '.1.html'#13#10#13#10'Software distributed under the License is distributed' +
          ' on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either expre' +
          'ss or implied. See the License for the specific language governi' +
          'ng rights and limitations under the License.'
        WordWrap = True
      end
    end
    object GroupBox2: TGroupBox
      Left = 16
      Top = 189
      Width = 554
      Height = 88
      Caption = ' Note '
      TabOrder = 1
      OnMouseMove = Label1MouseMove
      object Label8: TLabel
        Left = 16
        Top = 23
        Width = 412
        Height = 13
        Caption = 
          'Microsoft DMO Filters support only 8/16 Bit PCM and 32 Bit Float' +
          ' with 1 or 2 Channels.'
      end
      object Label9: TLabel
        Left = 16
        Top = 39
        Width = 247
        Height = 13
        Caption = 'Sound 3D Filter works only with 2 Channel Streams.'
      end
      object Label12: TLabel
        Left = 16
        Top = 55
        Width = 333
        Height = 13
        Caption = 
          'Audio Stream Delay will work only in combination with a video St' +
          'ream.'
      end
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 142
      Width = 317
      Height = 37
      Caption = ' Stream Information '
      TabOrder = 2
      OnMouseMove = Label1MouseMove
      object Label10: TLabel
        Left = 7
        Top = 15
        Width = 300
        Height = 13
        AutoSize = False
        Caption = '44100Hz - 16Bit (integer) - 2 Channels - SPDIF (bypassed)'
        Transparent = True
      end
    end
    object GroupBox4: TGroupBox
      Left = 456
      Top = 142
      Width = 114
      Height = 37
      Caption = ' Used Memory '
      TabOrder = 3
      OnMouseMove = Label1MouseMove
      object Label3: TLabel
        Left = 8
        Top = 15
        Width = 97
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
        Transparent = True
      end
    end
    object GroupBox5: TGroupBox
      Left = 340
      Top = 142
      Width = 109
      Height = 37
      Caption = ' CPU Usage '
      TabOrder = 4
      OnMouseMove = Label1MouseMove
      object Label11: TLabel
        Left = 11
        Top = 15
        Width = 86
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = '0'
        Transparent = True
      end
    end
    object GroupBox6: TGroupBox
      Left = 16
      Top = 16
      Width = 554
      Height = 116
      Caption = ' About '
      TabOrder = 5
      OnMouseMove = Label1MouseMove
      object label6: TLabel
        Left = 184
        Top = 24
        Width = 352
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'DC-DSP Filter v1.00 RC3 (Build on 20. June 2004)'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        OnMouseMove = Label1MouseMove
      end
      object Label1: TLabel
        Left = 274
        Top = 42
        Width = 140
        Height = 13
        Caption = '(C) Milenko "DCoder" Mitrovic'
        Transparent = True
        OnMouseMove = Label1MouseMove
      end
      object Label5: TLabel
        Left = 273
        Top = 74
        Width = 31
        Height = 13
        Caption = 'email :'
        Transparent = True
        OnMouseMove = Label1MouseMove
      end
      object Label7: TLabel
        Left = 273
        Top = 58
        Width = 30
        Height = 13
        Caption = 'web  :'
        Transparent = True
        OnMouseMove = Label1MouseMove
      end
      object label4: TLabel
        Left = 307
        Top = 74
        Width = 104
        Height = 13
        Cursor = crHandPoint
        Caption = 'dcoder@dsp-worx.de'
        Transparent = True
        OnClick = Label4Click
        OnMouseMove = Label4MouseMove
      end
      object statictext1: TLabel
        Left = 307
        Top = 58
        Width = 121
        Height = 13
        Cursor = crHandPoint
        Caption = 'http://www.dsp-worx.de'
        Transparent = True
        OnClick = StaticText1Click
        OnMouseMove = StaticText1MouseMove
      end
      object Label13: TLabel
        Left = 502
        Top = 93
        Width = 37
        Height = 13
        Alignment = taRightJustify
        Caption = 'Label13'
      end
      object Image2: TImage
        Left = 15
        Top = 21
        Width = 140
        Height = 80
        Cursor = crHandPoint
        AutoSize = True
        Picture.Data = {
          0A54504E474F626A65637489504E470D0A1A0A0000000D494844520000008C00
          0000500806000000CB3C9D6A00000006624B4744000000000000F943BB7F0000
          00097048597300000B1300000B1301009A9C180000000774494D450000000000
          00000973942E000013144944415478DAED5D09741455BAFE7B4D67276064098F
          2504223C0C06D0616604D9044451671CC791A3CCB8707C3E19E139C72833A263
          601E3E38C3636644069D6134FAF42110204294352C0A91880464886109498074
          129A0442F6EEAE7EF7BF55B7BBAABBAAB7F4C63BF59DDC732B55B7EAFFEBDEAF
          FEFFBF4B5769C0131A2189B7316965F6F9823F655484160E3F8E3B44E538997D
          0EA5EB6864B6C50461B9D6ED7F7F49A31226F270F838C61227CA39B7FFDDC9E3
          84C63D5FB76E6D7A76F66D0B525253672525268D059765517193C1E17058DBDA
          5ACB9B9B9B8E3436369ED8BD674FF907EF1734D8ED764610BB4C72271088724F
          AB72EC9BB272AD4E1747B6D31C1C976AB373FA68DFB88AE0A1D769EDA0D1B4EA
          74FA6B9D1D1D0D6BDE59935750F0611DF0E4B0899255B42D268EC43D496294C3
          5F7DF955AFB4DEE3BBBABAE83EC2D068DFAF8A1041A3D180D5DA7DF5DB63DFBE
          F3667E7EB1D96CEE049E18DD247509394B8C348C384ED2386392D77E9B37F3E9
          F9CFED686B6D8FF6BDA9081734C07577755D28DAB675D99BF9CB6A80270692A5
          43489DE0220FB338481A67EC8384D191A45DBBE64F7366CD7E70B3D56AF59073
          B6B202B6EF3F09A7AB2C606E925A1D8329D9A3BCB5F386A2CE6AF9F0979F34DA
          040F4CCE81E1D92365CFF9E668E9DFE63E316F1FF004692309AD44AB90B7838B
          384806897B42C2608CA2FBF8A382C7274EBAE71F361A0FB9B0E5B39DF0D7CDA7
          C11750716F37A6968F7CF97F7B6414FC64CE4CE7FF7C88E180A3A5476A7F31F7
          89FF069E2C489416926E0839230E238D38A6A1843190A4DFB8E193C584304BC4
          022B2ABF8705CBF752457273B261D6C4D1306C483AF4E9950C2A6217F97F2982
          E3272B3D08832041301C3A78C0412CCC5BE022C975929A85FCBAB09F9146E29A
          903046E009B3EDDE1933A78B2DCC92E56BE1E0A94E183D6200FCEE8539A0D3AA
          C32A3703EC9C03FEB0E6333875A60EDE5E3C0D4666DF26394E08038F3EF6F87B
          C013E31A494D42BA2AFC8FA4619686C533D4CA2003B00B6DD85AB8E9C6B47BA5
          6C9CF0B3B768BEECA587213BB37FB4EB414500A8AC32C36BABB6D27866E9E2E7
          9DFBD1C2ECDEB5130953082ECB8244B9429245D8C67D6879D065312BE3248C89
          242321CCF5FBEE9F23113AE1D1D5345FB7EC71488A57C7EF6E26B47670F0D4CB
          EBE876E9A65725C7F6EEDE090FFFF4677B81270C120489D228A42BC23E6665B0
          F7C4BADA9430F124C5EDF86C5BF3CCFBEE975CF8CE9FFE91E6EFAF7C22DAF7AF
          2208FCEAE58F685EBA719164FFE73B3E43C27C0DBCFB61D6A581A47AE0498304
          422B832E0B09E30C7E913009C013A669F6030F4A2E3CE1F1029ABFFBE6F468DF
          BB8A20C0085356F81BC9FE9D9FEF80FBE73C74025C16869185252410734B18C7
          38BBD88C30264298AB4A84599D3795E63ABD3AF27B33C06ED3D0B6FAF57F96D0
          FF4B3F9927395EBCBD080953013C61983B320B89599926E1B8B8B7445D5222F0
          16469130AB5E990C5A873486D959BC25A2953073F64FFC2A1711BD34F8A78119
          F73D1CD3F5B168C53E9A2B10E62CB85C125A18B32835822B8EC1C05762619030
          68612C4A8459BEE81E0F65F6ECDC06EF1D1914D14ADAF0FB5CAFC751279C3379
          F7F0BF445CA758AC8FC5AB0FD05C8130E7C1656118617052B25EF85F4C18674F
          C92B61263D5D44F3DFCD1FE3DCC791D3B40680925DDBE0FDF2F1A4DBC64124B0
          FF1B0BCD3F7CDDB392F6EDDC4A7324CBFA52BED1268FBF25223AE5CF8D87A159
          B7C5547D30BCF167AF84B9009E16A64E48CCC2E07165C2B8F792A6CCDF41F3BC
          5FE638F7E948A3D885596CADC6066347F58B4805212AAB9B21FFE33AF88F87FA
          7A1C7BE69D2BF0C18B7D2073602F48493446449FFF2D3E0506AE05060F1D1173
          F58158F1C1499A1F5C2F350422C2C8591833482D0C76ADE509337DC62CC98567
          2EE019FAEBC7863BF771A473A515AD903979EC4B1260E941A71312D9C6273D14
          C0B90FBBCD0676BB0D388EA3D7D568B4A0D16A207BD4588FF2C7CB0E80C11007
          7171A690EAE101725D2DD143ABD55119030767C5647DACFE1F9E3025EF490D81
          D04B6284115B98CBE02F61B6166EB2CC98355B72E1075E2AA5F9FC87877A28F3
          F5C1225A197ABD814FA47272C6DD1D96F63971EC10A9243BBF1643A78331E326
          3A8F1DDCBB9506A05417839110C608B977DE13BCB02071685F6CD487187FD970
          96E63BDF96D6C79E5D5F2061AA41DEC2309764015F84719F1A78E8E5329ACFBB
          7F888732A5078A60F3A9C11EFB57BE3006428DB2AF3E874FCB07C8CA3942F428
          8C901EDE102BF52146C18E6A9A6F5F3541B27FD717C5387087077B4698C953A6
          4A2EFCF33CDEA43D323303C026ED561FFDAA08B65FBC4B12E46120F6FCD438C8
          18941DD20A7297259613493D02D1315AF521C686FD3534DFB6F24EC97E616AA0
          1ABCC730DE09B371C3279629D3EE059BB5DB79E179AF9FA145674FEDC72FB372
          C3F0C13A499087416043DD55E8979105A1865896BB1C393DEA6AEB2163F0C8A0
          648542C770EBE1AD3E188A0FD5D3FCD3153992FDFB4BF68586301327497DDDBC
          BC33349F36395D51F173155F0B419E81069C3AE2BF6FED9F199606A9FAFE28E8
          880CA3D1E421E73C39468350AD96C6349CC30E43878F0B8B1EDE80F5817105D5
          25CC7A54559609719B49B6DEF71EBA42573B15E48F70EEC3F2257B77E36C7535
          F484301F7F54E0419867965EA0F9DD77788E697CF74D31A918035594EF111820
          33FB0761A918268B27A41186DD26F5C995A70ED0C6A1BD1192B037C17176D2A3
          B092E0D08ADD8BD02A84BD1392DD3EDED54910D707EB3D854B0F7C28F4A41E0C
          E4C1D113B20CC99227E49765FC584DC1F21192FDC27A986AE809613EFCE01F96
          1F4F9A44BB6B0CFFBEF432CDC7E5A4792873FADBCFE1B0C5D3373F3B23F4D645
          4E96BB9CBFEDAA0AB95C7FC0F450AA8F48EB21C6B18A669AFF7D89B4978B8499
          FBC4BC6AE82961EE9A30813E0D0C8B5636292A587F611F34A4CDF408F246C4DF
          80C4D4D00E91BBCB5292B3F0A90CF2946B61FD1775616A1629C47AC8D547B831
          71A481B497030AB7D7128BA70553A27CE8B0FAE5DECE6DB480474B4BE1C95F3E
          550D3D21CCDFDF5B67F9E18F7E08DD569BF3E2797F6C5154B6BE9A54501F19C2
          18C340183759DEE4AC5F3996B8C7D00FD8B5B47543D5A56B12328AF590AB0F6C
          D09C1103207B485A30227DC26C69831717975017159FD857B1DC8ADFA438B78D
          063D1C397C049E99FF5C35F49430B963C781CDE6EA25BDF676B7A2124A84C9D2
          3743725A68DD922C61E25A2131457E92B1F5DA051243E868ACD1537024F6B075
          B792DE632B3C3AF76E385EE3BA26AF471BD163A06C7D5C6D3340E7B9EF4983E2
          D07828E3288D30CACBDFA337B220962D704D9560EC73FCDB63B1439821DC4548
          EB9B03A144A08409071A6A0F417DDA548FFBCD36B543427286B2C51508154DC4
          34610676558083F40C389C740AE153654ECC75CE408B09837A44061AA2C31D4E
          1DD0DDAC2832C2D0EE33109FD4CF0B61224B6C39C408614A4805CDF0A8A07042
          4A18E60A4A6843460AA88333D8DC42EA5C881F94EA43B53002E42A285290BA82
          C8EA21268B46AB233D935B15EB43AC6734111384412C7C3623A2EB3F187028BC
          706B0D750591D6037B260B69CFC44516A5FA70D7335A080B61D6AEF9932577DC
          9D8430AE6E75FE3ADF4F6DDBF55A7E235C6B4F2450EE1D845F0FFF7A26540FB6
          4EC5CF5E4CB8F1FA73AE8963BD5E0FC78F95C1F32F2CAC8668104645EC43258C
          8A801033847175697937D06FC894A0CAF8530E834A105C02BFE4D15FD7E3A093
          800E879D4EFE79D7112FAB0DBF0C7A7F53FDBCB65C5D9538DD5EDF41937C968F
          29C2E0F88818B9E9690197F155AEBE663FCDCD09A159BDD6BFAD5CD2A872B243
          2D0371FC4AB3CF7AF005D415095D9790E3F775628A30BEE678FC9D07922B3794
          E86FEB6E23DDD80E67052D79B27F50F3336C2E68DEB2931E15EDDE250E760EC8
          9B0C06EC49E104292B130869A8952256853D385817FCE0E159D213530EACC346
          98DBC7E4427757A7F3E26FBD2D74C7E2156EA0863472BA1B19F4840C2983022A
          A3546E2867266469875AE330BA0F07CDE8FCCC19617EC68FF5250E07079CBD0B
          ACDD3720FF8D079DB3D96C9031B7771A345EFA12CCBDA73865874386F83E7FF1
          ABE9744E4AEEB822596A7897664ECE759205C783366FAEA6AE49B627D6C167AF
          2E700D8F18E34CF0DD89E3D1220C7932D367B891413AB2E94F19A572998E06B0
          59DBE152CA0FA4E71BDA213188C1B0868B87E0B179F73827102929ED9789556D
          87CBA93F0E9B8CE1BA16484A752D10CFC8D6C0CFA70D74926AB0B51A7AF7F5EE
          12B17ECCC9FC88367B70BACF9D07ADD6E0311E74531126DB201DD9F4A78C52B9
          611A0BD8AD1D509B34CEE7F97E3566ED01A8BF65BA5406A9176AC592C6874D86
          DCB5163EE7E69ABC5819DEBA682861D8D40475459D678865F13228186B846137
          2E5E8C8C263221A97FC065E4CA6DFCB4923C9943FC3EDF1FF82B63D3C6B31E6E
          335819858535B20DBB7E15BF7687FD8251DFD0A978CDC953B4F0C8BDFF4A7FD5
          49479B5F29A1F18CD741C158240CA2F9CA2972E346D0E94DC4441A6587C2FD29
          232EA725096383E45E99019DEF0F94657C075A8D9EC604B878BBD72DA3829671
          FD6A05B93EAE418E578E3144321DC08183B341DAADCA3DC16B967FD2EB611D60
          4FC9E70872AC1106CD2F2E7CD6EA4CB4017AA58F0EAA8CB7721673196D54F4D5
          DECEF707BE65E8853118A06B9BB1011D0E1B04F246741CBFD152329A20CD8BAE
          2E5DE2F82904003A8E834B43DC65D22909E1FEBD11CA03E124CCEA552B2C3963
          EE2041A6AB5BBDEA433D70EDA21FB6BB11076FBA2E49BA58CADD17FB53C65B39
          6CCC8B71593ECFF70781C80815947495D3A5A7D774A2C3B5A94DE886979E74B5
          A9DEA0879327CA61D14B79D51069C22094628240CBC895FB7443057517FE9EEF
          0FFC91110AD038E3D5FDD47A9812E417670723D3AFFB8F65C2209A1B4FD2310B
          F4D568DAE57CBF3F6510CCF7A30B42D39C94CAFF4482F97AFE77475C48E20B94
          C17156690CA335F02E85AEC3F5676AC0415717F2EE8BFF8E03DE271F23C579EF
          C510B4B508B3EC5E6539F8558C74EA81A3F2BCDE7FAC12C6DD0FCBF97E7E4980
          9E56A256CB8F38CAC5074ABEBFB9F104AD20FA43352FE7FB03251957EABEA60D
          4183D420E3A46B96D354274A1852174AD7C0E1034119E83768721032ACC2F257
          803EFD147E5119CB8409D60FFB82D84F97DB12C0D172396C3270B0AD2E71B4EC
          B14070BCA9D9EB35C4E329C1CAF1258322560983088FEF3F405D97D8F7373796
          C3968F9E0EC96F8EE464788E9DD406B5F0C9D7D80A1B4F69EBB0CADE67286444
          8C30A3468D86F676D7F7920E1EBB06A7CE657928E18EB61BA158ED265AD146B6
          E313E41BABF5063F7712DCE724BDCB686BBD28BC5D4AEF55075FA03A9258087F
          8D88F2E213A40F54477BBD53FF70C9600FF7E8AC7330695C2FE7EE848404387D
          FA547409A32206A1124645405009A32220A88451111054C2A80808E126CC7F2D
          5F6AC9CE1E09E28F845EBE7C118A0FDF15ED5B57D103CCFED151C8C870FDB6DB
          603040656505BCB2784935F4E42D9A4898CCCC2CE8EC74F5E9753A1D9C3F7F16
          0E7D372DDAF7AD22084CBC7D2F0C1B369CBED397C164324155D5394618A557C7
          FB7EB1F3D2379758F0E21D1D52DF63341AE1525D135C694D85B3557DA1CBAA7E
          2034961167B801C3331B203DE93A0C1CD01BBABBA53F778E8F8FA74660C91B4B
          7BF6EA7824CC8001038900CF555FF8D6CA949414EAFFD0A4A9885D6048817168
          4B4B8B425BC69150E352D3EBBF5F866408FEE3144898E464D57AFC7F074EDC9E
          3F5F55BB66EDBB4880A03F7F13F7DA6FF37611C244FEC5B62A220A9CDD3F7CB8
          B4AA687B310636417D608B7EC20F7362652E46FB86548417C45D719B366DA939
          7BBE0AAD46509FF0A31F0915F21462694E47FBA6548407384979FAFBCAF63DBB
          F799AFB7B460301BD44742E96788851C8398B4175F78F6639287E7FDEF2A228D
          CEF68E8EE6C6464BDB3F2BCE18EAEACCA427DC8964C114D46788E987CE81274C
          1249A924F521095F288773EFB70ADB7D84638942593C07D701B0B50672EB0D22
          F1962115523864FEE7841CE316EC67A3D5C0D824A80F9DA375C1BE321207C980
          6F01C6655C8C34E9C236EE131306CF41C2F08B4B54C2C40AE408C3C8C2088324
          6084414BC2AC8C45D866EE48D243028130CC52B03806AD8C9834BD85841312E8
          B268375C384F072A61620DFE10064980644097836EA949486CEC85599776A1BC
          8430CC52302B83A4416220699024CCB2E0FF49E0EA55B1F35497145B50220CBA
          25742BD8F84818B4324818B424CCD25C1712EE17F78E68FC82D7C106656E8559
          19461A66691879F07F665D981BC3F354C2C416BC11061B1D0980412CB3326849
          90348C3CCCB2748228D815CEA784618D8E3992404C1A4C89A2640297753108E7
          A984892D78230C262BB848C362192408238A982C4E570422C2B0066756068980
          F109234EBC90986561310F7347ECF5462A6162030E85FF59A3DB8484B109B334
          1D42624461718BD315B1C42C03230E0B62910C4651624461968595F3665D0054
          C244030E857D62B7C45C1326469C6E51B289CA71A2F39D0D2A260D230EB32206
          D1B6BB6511134E0E2A61220F6F8411BB269B5BB28AB6256E487C4D8D4CAE0529
          71DC93BF64015009130D38BCEC77278D5D267120258BE49AE20695B336EE0412
          EFF7E745B62A61220F878F6362D288C9C3B9ED77C85D4F6D501501E1FF006FF3
          14D5ECA3A3890000000049454E44AE426082}
        Transparent = True
        OnClick = StaticText1Click
      end
    end
  end
  object Timer1: TTimer
    Interval = 2000
    OnTimer = Timer1Timer
    Left = 688
    Top = 24
  end
end
