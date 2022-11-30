(* ::Package:: *)

(* ::Input:: *)
(*SetOptions[EvaluationNotebook[],StyleDefinitions->$UserBaseDirectory<>"/SystemFiles/FrontEnd/StyleSheets/my-stylesheet.nb"]*)


BeginPackage["MathematiDark`"];


(* ::Section:: *)
(*Public *)


ColorMyMenu::usage="Creates or overrides the file '$UserBaseDirectory/FrontEnd/frontend.css'.";
SetColors::usage
SetColors::badarg="Argument must be a list of RGBColors"
SetColors::length="It is advised for an entered list to have eight entries. Results may be unexpected."
GetColors::usage="Returns the seleced colors"
CreateStyleSheet::usage="Creates a Stylesheet with the set colors"
CreateStyleSheet::badarg="Argument must be a list of RGBColors"
CreateStyleSheet::argx="You need to add some colors first."
ApplyStyleSheet::usage="Applies the created StyleSheet to the current Notebook"
SaveStyleSheet::usage="Saves the creaded Stylesheet under $UserBaseDirectory/SystemFiles/FrontEnd/StyleSheets/maTHEMEatica.nb"
SetDefault::usage
CreateCSS::usage
CreateCSSPlay::usage
Test::usage
Test2::usage

maTHEMEatica::usage
StyleSheetToDefault::usage


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*General*)


(* ::Subsubsection:: *)
(*Setting Colors*)


$colors=<|
	"background"->RGBColor["#140516"],
	"fontcolor"->White,
	"primary"->RGBColor["#eaa0eb"],
	"variable"->RGBColor["#55f7df"],
	"module"->RGBColor["#e638e9"],
	"block"->RGBColor["#35baa7"],
	"error"->RGBColor["#f51292"],
	"headhighlight"->RGBColor["#02584c"]
|>;



schemes["Nord"]=<|
	"background"->RGBColor["#2e3440"],
	"fontcolor"->RGBColor["#d8dee9"],
	"primary"->RGBColor["#88c0d0"],
	"variable"->RGBColor["#8fbcbb"],
	"module"->RGBColor["#81a1c1"],
	"block"->RGBColor["#5e81ac"],
	"error"->RGBColor["#bf616a"],
	"headhighlight"->RGBColor["#b48ead"]
|>;

schemes["Solarized"]=<|
	"background"->RGBColor["#002b36"],
	"fontcolor"->RGBColor["#eee8d5"],
	"primary"->RGBColor["#b58900"],
	"variable"->RGBColor["#6c71c4"],
	"module"->RGBColor["#268bd2"],
	"block"->RGBColor["#cb4b16"],
	"error"->RGBColor["#dc322f"],
	"headhighlight"->RGBColor["#2aa198"]
|>;

schemes["Gruvbox"]=<|
	"background"->RGBColor["#282828"],
	"fontcolor"->RGBColor["#ebdbb2"],
	"primary"->RGBColor["#689d6a"],
	"variable"->RGBColor["#b16286"],
	"module"->RGBColor["#458588"],
	"block"->RGBColor["#98971a"],
	"error"->RGBColor["#cc241d"],
	"headhighlight"->RGBColor["#d65d0e"]
|>;


SetColors[colors_Association]/;AllTrue[colors,MatchQ[_RGBColor|_GrayLevel]]:=Module[{},
	$colors=Merge[{$colors,colors},Last];
]; 
SetColors[colors_List]/;AllTrue[colors,MatchQ[_RGBColor|_GrayLevel]]:=Module[{assoc},
	assoc = NewAssoc[colors];
	SetColors[assoc];
]; 
SetColors[scheme_String/;ValueQ@$colors[key]]:=
	$colors=schemes[scheme];
SetColors[key_String,color_RGBColor|color_GrayLevel]:=Module[{assoc},
	assoc = <|key->color|>;
	SetColors[assoc];
];
SetColors[foo_]:=Module[{},
	Message[SetColors::badarg];
	$Failed
];

Options[GetColors]={"Hex"->False};
GetColors[slot_Integer:All,OptionsPattern[]]:=Module[{hex,nohex},
	hex=Boole@OptionValue["Hex"];
	nohex=Boole@!OptionValue["Hex"];
	(hex ColorToHex@# + nohex #)&@$colors[[slot]]
];
GetColors[key_String,OptionsPattern[]]:=Module[{hex,nohex},
	hex=Boole@OptionValue["Hex"];
	nohex=Boole@!OptionValue["Hex"];
	(hex ColorToHex[#] + nohex #)&@$colors[key]
];


NewAssoc[colors_List]:=Module[{diff,keys,cutKeys,cutColors},
	keys={"background","fontcolor","primary","variable"};
	diff=Length@colors-Length@keys;
	cutKeys=If[diff<0,
		Message[SetColors::length];
		Drop[#,diff],
		#]&@keys;
	cutColors=If[diff>0,
		Message[SetColors::length];
		Drop[#,-diff],
		#]&@colors;
	<|cutKeys->cutColors//Thread|>
]


ColorToHex[color_RGBColor]:=Module[{red,green,blue},
	red=StringPadLeft[#,2,"0"]&@IntegerString[#,16]&@Round@(255 color[[1]]);
	green=StringPadLeft[#,2,"0"]&@IntegerString[#,16]&@Round@(255 color[[2]]);
	blue=StringPadLeft[#,2,"0"]&@IntegerString[#,16]&@Round@(255 color[[3]]);
	"#"<>red<>green<>blue
]
ColorToHex[color_GrayLevel]:=Module[{gray},
	gray=StringPadLeft[#,2,"0"]&@IntegerString[#,16]&@Round@(255 color[[1]]);
	"#"<>gray<>gray<>gray
]
Attributes[ColorToHex]={Listable};


(* ::Subsubsection:: *)
(*Summary*)


maTHEMEatica[colors_List:_]:=Module[{},
	CreateStyleSheet[colors];
	ApplyStyleSheet[];
	SaveStyleSheet[];
	ColorMyMenu[];
]


(* ::Subsubsection:: *)
(*Cleanup*)


DeleteStyleSheet[]:=DeleteFile[FileNameJoin[{$UserBaseDirectory,"SystemFiles","FrontEnd","StyleSheets","maTHEMEatica.nb"}]];
DeleteCSS[]:=DeleteFile[FileNameJoin[{$UserBaseDirectory,"FrontEnd","frontend.css"}]];
DeleteAll[]:=Module[{},
	DeleteStyleSheet[];
	DeleteCSS[];
	SetOptions[$FrontEnd,DefaultStyleDefinitions->"Default.nb"];
];


(* ::Subsection:: *)
(*Stylesheet*)


Options[CreateStyleSheet]={"LightMode"->False};
CreateStyleSheet[OptionsPattern[]]:=Module[{light,dark,fallback,fontcolor},
	Echo["Creating StyleSheet based on the colorscheme:"];
	Echo[List@@GetColors[]];
	$stylesheet=Notebook[{
	(*Setting Default inheritance to Defa.nb*)
		{light,dark}=Boole@{#,!#}&@OptionValue["LightMode"];
			fallback = light "Default.nb" + dark "ReverseColor.nb";
		Cell[StyleData[StyleDefinitions -> fallback]],
	(*Default settings for All and Notebook*)
		Cell[StyleData["Notebook"],
			CellBracketOptions->{"HoverColor"->GetColors["primary"]},
			AutoStyleOptions->{
				"LocalVariableStyle"->{FontColor -> GetColors["module"]},
				"FunctionLocalVariableStyle"->{FontColor -> GetColors["block"]},
				"PatternVariableStyle"->{FontColor -> GetColors["variable"], FontSlant -> "Italic"},
				"UndefinedSymbolStyle"->{FontColor -> GetColors["primary"]}},
			CodeAssistOptions->{
				"HeadHighlightStyle"->{Background -> GetColors["headhighlight"], GetColors["fontcolor"]},
				"MatchHighlightStyle"->{Background -> GetColors["primary"], FontColor -> GrayLevel[0]},
				"MenuDefaultFillColor"->GetColors["primary"],
				"MenuDarkFillColor"->Black,
				"MenuBorderColor"->Black
				},
			FontColor->GetColors["fontcolor"],
			Background->GetColors["background"]],  
	(*Sections and Items*) 
		Cell[StyleData["Input"],
			FontColor->GetColors["fontcolor"]], 
		Cell[StyleData["Section"],
			FontColor->GetColors["primary"]], 
		Cell[StyleData["Subsection"],
			FontColor->GetColors["variable"]], 
		Cell[StyleData["Subsubsection"],
			FontColor->GetColors["module"]],
		Cell[StyleData["Item"],
			CellDingbat->StyleBox["\[FilledSmallSquare]", GetColors["primary"]]],
		Cell[StyleData["MessageMenuLabel"], 
			FontColor->GetColors["error"]],
	(*Trying to make information boxes somewhat bearable*)	
		Cell[StyleData["InformationTitleText"],
			FontColor->GetColors[6],
			Background->GetColors["background"]],
		Cell[StyleData["InformationUsageText"],
			FontColor->GetColors["fontcolor"],
			Background->GetColors["background"]],
		Cell[StyleData["InformationRowLabel"],
			FontColor->GetColors[1]],
		Cell[StyleData["DialogStyle"],
			Background->GetColors["background"],
			FontColor->GetColors["fontcolor"]],
		Cell[StyleData["TI"],
			Background->GetColors["background"],
			FontColor->GetColors["fontcolor"]],
		Cell[StyleData["TR"],
			Background->GetColors["background"],
			FontColor->GetColors["fontcolor"]],
		Cell[StyleData["Column"],
			Background->GetColors["background"],
			FontColor->GetColors["fontcolor"]],
		Cell[StyleData["Row"],
			Background->GetColors["background"],
			FontColor->GetColors["fontcolor"]]
	(*Setting options for Plots*)
		(*Cell[SetOptions[
			{
				Plot, 
				ParametricPlot, 
				ListPlot, 
				ListLogPlot, 
				ListLogLinearPlot, 
				ListLogLogPlot
			},
			PlotStyle->List@@GetColors[]]],
		Cell[SetOptions[Hyperlink,BaseStyle->Red,ActiveStyle->Green]]*)
	},StyleDefinitions->"PrivateStylesheetFormatting.nb"];
];

CreateStyleSheet[colors_]:=Module[{},
	SetColors[colors];
	CreateStyleSheet[];
]
	


PlayAround[]:=Module[{},
	Manipulate[CreateStyleSheet[{Hue[x],RGBColor["#35baa7"],RGBColor["#e638e9"],RGBColor["#55f7df"],RGBColor["#bf59de"],RGBColor["#bc0bf3"]}],{z,0,1}]
	ApplyStyleSheet;
]


ApplyStyleSheet[]:=SetOptions[EvaluationNotebook[],StyleDefinitions->$stylesheet];

SaveStyleSheet[]:=Module[{},
	Export[FileNameJoin[{$UserBaseDirectory,"SystemFiles","FrontEnd","StyleSheets","maTHEMEatica.nb"}],$stylesheet];
];

Test[]:= NotebookObject@@$stylesheet;
Test2[]:=NotebookClose[$stylesheet];

SetDefault[]:=SetOptions[$FrontEnd,DefaultStyleDefinitions->$UserBaseDirectory<>"/SystemFiles/FrontEnd/StyleSheets/maTHEMEatica.nb"]



(* ::Subsection:: *)
(*Menu Bars*)


Options[CreateCSS]={"LightMode"->False}
CreateCSS[slot_:1,OptionsPattern[]]:=Module[{},
	$cssString=
	"
* {
	background: #191919;
	color: #DDDDDD;
	border: 1px solid #5A5A5A;
}

QWidget::item:selected {
	background: "<>GetColors[slot,"Hex"->True]<>";
}

QCheckBox, QRadioButton {
	border: none;
}

QRadioButton::indicator, QCheckBox::indicator {
	width: 13px;
	height: 13px;
}

QRadioButton::indicator::unchecked, QCheckBox::indicator::unchecked {
	border: 1px solid #5A5A5A;
	background: none;
}

QRadioButton::indicator:unchecked:hover, QCheckBox::indicator:unchecked:hover {
	border: 1px solid #DDDDDD;
}

QRadioButton::indicator::checked, QCheckBox::indicator::checked {
	border: 1px solid #5A5A5A;
	background: #5A5A5A;
}

QRadioButton::indicator:checked:hover, QCheckBox::indicator:checked:hover {
	border: 1px solid #DDDDDD;
	background: #DDDDDD;
}

QGroupBox {
	margin-top: 6px;
}

QGroupBox::title {
	top: -7px;
	left: 7px;
}

QScrollBar {
	border: 1px solid #5A5A5A;
	background: #191919;
}

QScrollBar:horizontal {
	height: 15px;
	margin: 0px 0px 0px 32px;
}

QScrollBar:vertical {
	width: 15px;
	margin: 32px 0px 0px 0px;
}

QScrollBar::handle {
	background: #353535;
	border: 1px solid #5A5A5A;
}

QScrollBar::handle:horizontal {
	border-width: 0px 1px 0px 1px;
}

QScrollBar::handle:vertical {
	border-width: 1px 0px 1px 0px;
}

QScrollBar::handle:horizontal {
	min-width: 20px;
}

QScrollBar::handle:vertical {
	min-height: 20px;
}

QScrollBar::add-line, QScrollBar::sub-line {
	background:#353535;
	border: 1px solid #5A5A5A;
	subcontrol-origin: margin;
}

QScrollBar::add-line {
	position: absolute;
}

QScrollBar::add-line:horizontal {
	width: 15px;
	subcontrol-position: left;
	left: 15px;
}

QScrollBar::add-line:vertical {
	height: 15px;
	subcontrol-position: top;
	top: 15px;
}

QScrollBar::sub-line:horizontal {
	width: 15px;
	subcontrol-position: top left;
}

QScrollBar::sub-line:vertical {
	height: 15px;
	subcontrol-position: top;
}

QScrollBar:left-arrow, QScrollBar::right-arrow, QScrollBar::up-arrow, QScrollBar::down-arrow {
	border: 1px solid #5A5A5A;
	width: 3px;
	height: 3px;
}

QScrollBar::add-page, QScrollBar::sub-page {
	background: none;
}

QAbstractButton:hover {
	background: #353535;
}

QAbstractButton:pressed {
	background: #5A5A5A;
}

QAbstractItemView {
	show-decoration-selected: 1;
	selection-background-color: "<>GetColors[slot,"Hex"->True]<>";
	selection-color: #DDDDDD;
	alternate-background-color: #353535;
}

QHeaderView {
	border: 1px solid #5A5A5A;
}

QHeaderView::section {
	background: #191919;
	border: 1px solid #5A5A5A;
	padding: 4px;
}

QHeaderView::section:selected, QHeaderView::section::checked {
	background: #353535;
}

QTableView {
	gridline-color: #5A5A5A;
}

QTabBar {
	margin-left: 2px;
}

QTabBar::tab {
	border-radius: 0px;
	padding: 4px;
	margin: 4px;
}

QTabBar::tab:selected {
	background: #353535;
}

QComboBox::down-arrow {
	border: 1px solid #5A5A5A;
	background: #353535;
}

QComboBox::drop-down {
	border: 1px solid #5A5A5A;
	background: #353535;
}

QComboBox::down-arrow {
	width: 3px;
	height: 3px;
	border: 1px solid #5A5A5A;
}

QAbstractSpinBox {
	padding-right: 15px;
}

QAbstractSpinBox::up-button, QAbstractSpinBox::down-button {
	border: 1px solid #5A5A5A;
	background: #353535;
	subcontrol-origin: border;
}

QAbstractSpinBox::up-arrow, QAbstractSpinBox::down-arrow {
	width: 3px;
	height: 3px;
	border: 1px solid #5A5A5A;
}

QSlider {
	border: none;
}

QSlider::groove:horizontal {
	height: 5px;
	margin: 4px 0px 4px 0px;
}

QSlider::groove:vertical {
	width: 5px;
	margin: 0px 4px 0px 4px;
}

QSlider::handle {
	border: 1px solid #5A5A5A;
	background: #353535;
}

QSlider::handle:horizontal {
	width: 15px;
	margin: -4px 0px -4px 0px;
}

QSlider::handle:vertical {
	height: 15px;
	margin: 0px -4px 0px -4px;
}

QSlider::add-page:vertical, QSlider::sub-page:horizontal {
	background: "<>GetColors[slot,"Hex"->True]<>";
}

QSlider::sub-page:vertical, QSlider::add-page:horizontal {
	background: #353535;
}

QLabel {
	border: none;
}

QProgressBar {
	text-align: center;
}

QProgressBar::chunk {
	width: 1px;
	background-color: "<>GetColors[slot,"Hex"->True]<>";
}

QMenu::separator {
	background: #353535;
}
";
];

CreateCSS[arg_:"primary","LightMode"->True]:=Module[{},
	$cssString=
	"
QWidget::item:selected {
	background: "<>GetColors[arg,"Hex"->True]<>";
}
QAbstractItemView {
	selection-background-color: "<>GetColors["background","Hex"->True]<>";
}
	";
];


CreateCSSPlay[arg_:"primary",OptionsPattern[]]:=Module[{},
	$cssString=
	"
* {
	background: "<>GetColors["background","Hex"->True]<>";
	color: #DDDDDD;
	border: 1px solid #5A5A5A;
}
QWidget::item:selected {
	background: "<>GetColors[arg,"Hex"->True]<>";
}
QWidget::item:disabled {
	color: #888888;
}
QScrollBar {
		border: 0px;
		background: #000000;
}
QScrollBar::handle {
	background: #353535;
	border: 1px solid #5A5A5A;
}

QScrollBar::handle:horizontal {
	border-width: 0px 1px 0px 1px;
}

QScrollBar::handle:vertical {
	border-width: 1px 0px 1px 0px;
}

QScrollBar::handle:horizontal {
	min-width: 20px;
}

QScrollBar::handle:vertical {
	min-height: 20px;
}

QScrollBar::add-line, QScrollBar::sub-line {
	background: "<>GetColors["background","Hex"->True]<>";
	border: 1px solid #5A5A5A;
	subcontrol-origin: margin;
}

QScrollBar::add-line {
	position: absolute;
}

QScrollBar::add-line:horizontal {
	width: 15px;
	subcontrol-position: left;
	left: 15px;
}

QScrollBar::add-line:vertical {
	height: 15px;
	subcontrol-position: top;
	top: 15px;
}

QScrollBar::sub-line:horizontal {
	width: 15px;
	subcontrol-position: top left;
}

QScrollBar::sub-line:vertical {
	height: 15px;
	subcontrol-position: top;
}

QScrollBar:left-arrow, QScrollBar::right-arrow, QScrollBar::up-arrow, QScrollBar::down-arrow {
	border: 1px solid #5A5A5A;
	width: 3px;
	height: 3px;
}
QScrollBar::add-page, QScrollBar::sub-page {
	background: none;
}

";
];



ColorMyMenu[]:=Module[{check,file},
	check=FileExistsQ[FileNameJoin[{$UserBaseDirectory,"FrontEnd","frontend.css"}]];
	If[check,check=ChoiceDialog["The file "<>$UserBaseDirectory<>"/FrontEnd/frontend.css already exists. Do you want to override it?",{"Yes"->True,"No"->False},Background->GetColors[1]]];
	If[!check,Abort[]];
	file=OpenWrite[FileNameJoin[{$UserBaseDirectory,"FrontEnd","frontend.css"}]];
	WriteString[file,$cssString];
	Close[file]
];


(* ::Subsection:: *)
(*Ending Package*)


End[];
EndPackage[];
