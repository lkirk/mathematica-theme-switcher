(* ::Package:: *)

BeginPackage["MathematiDark`"];


(* ::Section:: *)
(*Public *)


ColorMyMenu::usage="Creates or overrides the file '$UserBaseDirectory/FrontEnd/frontend.css'.";
SetColors::usage
SetColors::badarg="Argument must be a list of RGBColors"
GetColors::usage="Returns the seleced colors"
CreateStyleSheet::usage="Creates a Stylesheet with the set colors"
CreateStyleSheet::badarg="Argument must be a list of RGBColors"
CreateStyleSheet::argx="You need to add some colors first."
ApplyStyleSheet::usage="Applies the created StyleSheet to the current Notebook"
SaveStyleSheet::usage="Saves the creaded Stylesheet under $UserBaseDirectory/SystemFiles/FrontEnd/StyleSheets/maTHEMEatica.nb"


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Setting Colors*)


$colors={RGBColor["#d49721"],RGBColor["#f31257"],RGBColor["#0397b1"],RGBColor["#a712ea"]};


SetColors[colors_List]/;AllTrue[colors,MatchQ[_RGBColor]]:=Module[{},
	$colors=colors
];
SetColors[foo_]:=Module[{},
	Message[SetColors::badarg];
	$Failed
];
Options[GetColors]={"Hex"->False};
GetColors[OptionsPattern[]]:=$colors;
GetColors[OptionsPattern[]]/;OptionValue["Hex"]:=ColorToHex/@$colors;
GetColors[slot_Integer,OptionsPattern[]]:=$colors[[Mod[slot,Length@$colors]+1]];
GetColors[slot_Integer,OptionsPattern[]]:=ColorToHex@$colors[[Mod[slot,Length@$colors]+1]]/;OptionValue["Hex"];


ColorToHex[color_RGBColor]:=Module[{red,green,blue},
	red=StringPadLeft[#,2,"0"]&@IntegerString[#,16]&@Round@(255 color[[1]]);
	green=StringPadLeft[#,2,"0"]&@IntegerString[#,16]&@Round@(255 color[[2]]);
	blue=StringPadLeft[#,2,"0"]&@IntegerString[#,16]&@Round@(255 color[[3]]);
	"#"<>red<>green<>blue
]


(* ::Subsection:: *)
(*Stylesheet*)


CreateStyleSheet[]:=Module[{},
	Echo["Creating StyleSheet based on the colorscheme:"];
	Echo[GetColors[]];
	$stylesheet=Notebook[{
		Cell[StyleData[StyleDefinitions -> FrontEnd`FileName[{$InstallationDirectory, "SystemFiles", "FrontEnd", "StyleSheets"}, "ReverseColor.nb", CharacterEncoding -> "UTF-8"]]],
		Cell[StyleData[All], 
			FontColor->White, Background->GrayLevel[.1]],
		Cell[StyleData["Notebook"],
			CellBracketOptions->{"HoverColor"->GetColors[1]},
			AutoStyleOptions->{
				"LocalVariableStyle"->{FontColor -> GetColors[3]},
				"PatternVariableStyle"->{FontColor -> GetColors[2], FontSlant -> "Italic"},
				"UndefinedSymbolStyle"->{FontColor -> GetColors[1]}},
			CodeAssistOptions->{"HeadHighlightStyle"->{Background -> GetColors[5], FontColor -> GrayLevel[0]}},
			FontColor->GrayLevel[1],
			Background->GrayLevel[.1]],
		Cell[StyleData["Section"],
			FontColor->GetColors[1]], 
		Cell[StyleData["Subsection"],
			FontColor->GetColors[2]], 
		Cell[StyleData["Subsubsection"],
			FontColor->GetColors[3]],
		Cell[StyleData["Item"],
			CellDingbat->StyleBox["\[FilledSmallSquare]", GetColors[1]]]
	},StyleDefinitions->"PrivateStylesheetFormatting.nb"];
];

CreateStyleSheet[colors_List]:=Module[{},
	SetColors[colors];
	CreateStyleSheet[];
]
	


ApplyStyleSheet[]:=SetOptions[EvaluationNotebook[],StyleDefinitions->$stylesheet];	

SaveStyleSheet[]:=Module[{},
	NotebookSave[$stylesheet,FileNameJoin[{$UserBaseDirectory,"SystemFiles","FrontEnd","StyleSheets","maTHEMEatica.nb"}]];
];


(* ::Subsection:: *)
(*Menu Bars*)


ColorMyMenu[]:=Module[{file},
	file=OpenWrite[FileNameJoin[{$UserBaseDirectory,"FrontEnd","frontend.css"}]];
	WriteString[file,
	"
* {
	background: #191919;
	color: #DDDDDD;
	border: 1px solid #5A5A5A;
}

QWidget::item:selected {
	background: "<>GetColors[1,"Hex"->True]<>";
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
	selection-background-color: "<>GetColors[1,"Hex"->True]<>";
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
	background: "<>GetColors[1,"Hex"->True]<>";
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
	background-color: "<>GetColors[1,"Hex"->True]<>";
}

QMenu::separator {
	background: #353535;
}"
	];
	Close[file]
];


(* ::Subsection:: *)
(*Ending Package*)


End[];
EndPackage[];
