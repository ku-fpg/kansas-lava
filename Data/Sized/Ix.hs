{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Data.Sized.Ix 
	( X0
	, X1
	, X2
	, X3
	, X4
	, X5
	, X6
	, X7
	, X8
	, X9
	, X10
	, X11
	, X12
	, X13
	, X14
	, X15
	, X16
	, X17
	, X18
	, X19
	, X20
	, X21
	, X22
	, X23
	, X24
	, X25
	, X26
	, X27
	, X28
	, X29
	, X30
	, X31
	, X32
	, X33
	, X34
	, X35
	, X36
	, X37
	, X38
	, X39
	, X40
	, X41
	, X42
	, X43
	, X44
	, X45
	, X46
	, X47
	, X48
	, X49
	, X50
	, X51
	, X52
	, X53
	, X54
	, X55
	, X56
	, X57
	, X58
	, X59
	, X60
	, X61
	, X62
	, X63
	, X64
	, X65
	, X66
	, X67
	, X68
	, X69
	, X70
	, X71
	, X72
	, X73
	, X74
	, X75
	, X76
	, X77
	, X78
	, X79
	, X80
	, X81
	, X82
	, X83
	, X84
	, X85
	, X86
	, X87
	, X88
	, X89
	, X90
	, X91
	, X92
	, X93
	, X94
	, X95
	, X96
	, X97
	, X98
	, X99
	, X100
	, X101
	, X102
	, X103
	, X104
	, X105
	, X106
	, X107
	, X108
	, X109
	, X110
	, X111
	, X112
	, X113
	, X114
	, X115
	, X116
	, X117
	, X118
	, X119
	, X120
	, X121
	, X122
	, X123
	, X124
	, X125
	, X126
	, X127
	, X128
	, X129
	, X130
	, X131
	, X132
	, X133
	, X134
	, X135
	, X136
	, X137
	, X138
	, X139
	, X140
	, X141
	, X142
	, X143
	, X144
	, X145
	, X146
	, X147
	, X148
	, X149
	, X150
	, X151
	, X152
	, X153
	, X154
	, X155
	, X156
	, X157
	, X158
	, X159
	, X160
	, X161
	, X162
	, X163
	, X164
	, X165
	, X166
	, X167
	, X168
	, X169
	, X170
	, X171
	, X172
	, X173
	, X174
	, X175
	, X176
	, X177
	, X178
	, X179
	, X180
	, X181
	, X182
	, X183
	, X184
	, X185
	, X186
	, X187
	, X188
	, X189
	, X190
	, X191
	, X192
	, X193
	, X194
	, X195
	, X196
	, X197
	, X198
	, X199
	, X200
	, X201
	, X202
	, X203
	, X204
	, X205
	, X206
	, X207
	, X208
	, X209
	, X210
	, X211
	, X212
	, X213
	, X214
	, X215
	, X216
	, X217
	, X218
	, X219
	, X220
	, X221
	, X222
	, X223
	, X224
	, X225
	, X226
	, X227
	, X228
	, X229
	, X230
	, X231
	, X232
	, X233
	, X234
	, X235
	, X236
	, X237
	, X238
	, X239
	, X240
	, X241
	, X242
	, X243
	, X244
	, X245
	, X246
	, X247
	, X248
	, X249
	, X250
	, X251
	, X252
	, X253
	, X254
	, X255
	, X256
	, Size(..)
	, Index
	, Row
	, Column
	) where
	
import Language.Haskell.TH
import Data.Sized.Ix.TH
import Data.Ix

--- because of TH's lack of type families, will be added later.
type family Index a
type family Row a
type family Column a

class (Eq ix, Ord ix, Show ix, Ix ix, Bounded ix) => Size ix where
	-- | return the size (number of possible elements) in type 'ix'.
	size     :: ix -> Int
	-- | add an arbitary index to a specific 'ix' position.
	addIndex :: ix -> Index ix -> ix
	-- | look at an 'ix' as an 'Index', typically just an 'Int'.
	toIndex  :: ix -> Index ix
	-- | project any 2D array position onto any array. Helper method for 'show'.
	seeIn2D	 :: (Row ix, Column ix) -> ix

type instance Index (a,b) = (Index a,Index b)
type instance Row (a,b)  = a
type instance Column (a,b)  = b

instance (Size x, Size y) => Size (x,y) where
	size (a,b) = size a * size b
	addIndex (a,b) (a',b') = (addIndex a a',addIndex b b')
	toIndex (a,b) = (toIndex a, toIndex b)
	seeIn2D = id

$(sizedTypeGenForUpto 256)

type instance Index X0 = Int
type instance Index X1 = Int
type instance Index X2 = Int
type instance Index X3 = Int
type instance Index X4 = Int
type instance Index X5 = Int
type instance Index X6 = Int
type instance Index X7 = Int
type instance Index X8 = Int
type instance Index X9 = Int
type instance Index X10 = Int
type instance Index X11 = Int
type instance Index X12 = Int
type instance Index X13 = Int
type instance Index X14 = Int
type instance Index X15 = Int
type instance Index X16 = Int
type instance Index X17 = Int
type instance Index X18 = Int
type instance Index X19 = Int
type instance Index X20 = Int
type instance Index X21 = Int
type instance Index X22 = Int
type instance Index X23 = Int
type instance Index X24 = Int
type instance Index X25 = Int
type instance Index X26 = Int
type instance Index X27 = Int
type instance Index X28 = Int
type instance Index X29 = Int
type instance Index X30 = Int
type instance Index X31 = Int
type instance Index X32 = Int
type instance Index X33 = Int
type instance Index X34 = Int
type instance Index X35 = Int
type instance Index X36 = Int
type instance Index X37 = Int
type instance Index X38 = Int
type instance Index X39 = Int
type instance Index X40 = Int
type instance Index X41 = Int
type instance Index X42 = Int
type instance Index X43 = Int
type instance Index X44 = Int
type instance Index X45 = Int
type instance Index X46 = Int
type instance Index X47 = Int
type instance Index X48 = Int
type instance Index X49 = Int
type instance Index X50 = Int
type instance Index X51 = Int
type instance Index X52 = Int
type instance Index X53 = Int
type instance Index X54 = Int
type instance Index X55 = Int
type instance Index X56 = Int
type instance Index X57 = Int
type instance Index X58 = Int
type instance Index X59 = Int
type instance Index X60 = Int
type instance Index X61 = Int
type instance Index X62 = Int
type instance Index X63 = Int
type instance Index X64 = Int
type instance Index X65 = Int
type instance Index X66 = Int
type instance Index X67 = Int
type instance Index X68 = Int
type instance Index X69 = Int
type instance Index X70 = Int
type instance Index X71 = Int
type instance Index X72 = Int
type instance Index X73 = Int
type instance Index X74 = Int
type instance Index X75 = Int
type instance Index X76 = Int
type instance Index X77 = Int
type instance Index X78 = Int
type instance Index X79 = Int
type instance Index X80 = Int
type instance Index X81 = Int
type instance Index X82 = Int
type instance Index X83 = Int
type instance Index X84 = Int
type instance Index X85 = Int
type instance Index X86 = Int
type instance Index X87 = Int
type instance Index X88 = Int
type instance Index X89 = Int
type instance Index X90 = Int
type instance Index X91 = Int
type instance Index X92 = Int
type instance Index X93 = Int
type instance Index X94 = Int
type instance Index X95 = Int
type instance Index X96 = Int
type instance Index X97 = Int
type instance Index X98 = Int
type instance Index X99 = Int
type instance Index X100 = Int
type instance Index X101 = Int
type instance Index X102 = Int
type instance Index X103 = Int
type instance Index X104 = Int
type instance Index X105 = Int
type instance Index X106 = Int
type instance Index X107 = Int
type instance Index X108 = Int
type instance Index X109 = Int
type instance Index X110 = Int
type instance Index X111 = Int
type instance Index X112 = Int
type instance Index X113 = Int
type instance Index X114 = Int
type instance Index X115 = Int
type instance Index X116 = Int
type instance Index X117 = Int
type instance Index X118 = Int
type instance Index X119 = Int
type instance Index X120 = Int
type instance Index X121 = Int
type instance Index X122 = Int
type instance Index X123 = Int
type instance Index X124 = Int
type instance Index X125 = Int
type instance Index X126 = Int
type instance Index X127 = Int
type instance Index X128 = Int
type instance Index X129 = Int
type instance Index X130 = Int
type instance Index X131 = Int
type instance Index X132 = Int
type instance Index X133 = Int
type instance Index X134 = Int
type instance Index X135 = Int
type instance Index X136 = Int
type instance Index X137 = Int
type instance Index X138 = Int
type instance Index X139 = Int
type instance Index X140 = Int
type instance Index X141 = Int
type instance Index X142 = Int
type instance Index X143 = Int
type instance Index X144 = Int
type instance Index X145 = Int
type instance Index X146 = Int
type instance Index X147 = Int
type instance Index X148 = Int
type instance Index X149 = Int
type instance Index X150 = Int
type instance Index X151 = Int
type instance Index X152 = Int
type instance Index X153 = Int
type instance Index X154 = Int
type instance Index X155 = Int
type instance Index X156 = Int
type instance Index X157 = Int
type instance Index X158 = Int
type instance Index X159 = Int
type instance Index X160 = Int
type instance Index X161 = Int
type instance Index X162 = Int
type instance Index X163 = Int
type instance Index X164 = Int
type instance Index X165 = Int
type instance Index X166 = Int
type instance Index X167 = Int
type instance Index X168 = Int
type instance Index X169 = Int
type instance Index X170 = Int
type instance Index X171 = Int
type instance Index X172 = Int
type instance Index X173 = Int
type instance Index X174 = Int
type instance Index X175 = Int
type instance Index X176 = Int
type instance Index X177 = Int
type instance Index X178 = Int
type instance Index X179 = Int
type instance Index X180 = Int
type instance Index X181 = Int
type instance Index X182 = Int
type instance Index X183 = Int
type instance Index X184 = Int
type instance Index X185 = Int
type instance Index X186 = Int
type instance Index X187 = Int
type instance Index X188 = Int
type instance Index X189 = Int
type instance Index X190 = Int
type instance Index X191 = Int
type instance Index X192 = Int
type instance Index X193 = Int
type instance Index X194 = Int
type instance Index X195 = Int
type instance Index X196 = Int
type instance Index X197 = Int
type instance Index X198 = Int
type instance Index X199 = Int
type instance Index X200 = Int
type instance Index X201 = Int
type instance Index X202 = Int
type instance Index X203 = Int
type instance Index X204 = Int
type instance Index X205 = Int
type instance Index X206 = Int
type instance Index X207 = Int
type instance Index X208 = Int
type instance Index X209 = Int
type instance Index X210 = Int
type instance Index X211 = Int
type instance Index X212 = Int
type instance Index X213 = Int
type instance Index X214 = Int
type instance Index X215 = Int
type instance Index X216 = Int
type instance Index X217 = Int
type instance Index X218 = Int
type instance Index X219 = Int
type instance Index X220 = Int
type instance Index X221 = Int
type instance Index X222 = Int
type instance Index X223 = Int
type instance Index X224 = Int
type instance Index X225 = Int
type instance Index X226 = Int
type instance Index X227 = Int
type instance Index X228 = Int
type instance Index X229 = Int
type instance Index X230 = Int
type instance Index X231 = Int
type instance Index X232 = Int
type instance Index X233 = Int
type instance Index X234 = Int
type instance Index X235 = Int
type instance Index X236 = Int
type instance Index X237 = Int
type instance Index X238 = Int
type instance Index X239 = Int
type instance Index X240 = Int
type instance Index X241 = Int
type instance Index X242 = Int
type instance Index X243 = Int
type instance Index X244 = Int
type instance Index X245 = Int
type instance Index X246 = Int
type instance Index X247 = Int
type instance Index X248 = Int
type instance Index X249 = Int
type instance Index X250 = Int
type instance Index X251 = Int
type instance Index X252 = Int
type instance Index X253 = Int
type instance Index X254 = Int
type instance Index X255 = Int
type instance Index X256 = Int

type instance Row X0 = X1
type instance Row X1 = X1
type instance Row X2 = X1
type instance Row X3 = X1
type instance Row X4 = X1
type instance Row X5 = X1
type instance Row X6 = X1
type instance Row X7 = X1
type instance Row X8 = X1
type instance Row X9 = X1
type instance Row X10 = X1
type instance Row X11 = X1
type instance Row X12 = X1
type instance Row X13 = X1
type instance Row X14 = X1
type instance Row X15 = X1
type instance Row X16 = X1
type instance Row X17 = X1
type instance Row X18 = X1
type instance Row X19 = X1
type instance Row X20 = X1
type instance Row X21 = X1
type instance Row X22 = X1
type instance Row X23 = X1
type instance Row X24 = X1
type instance Row X25 = X1
type instance Row X26 = X1
type instance Row X27 = X1
type instance Row X28 = X1
type instance Row X29 = X1
type instance Row X30 = X1
type instance Row X31 = X1
type instance Row X32 = X1
type instance Row X33 = X1
type instance Row X34 = X1
type instance Row X35 = X1
type instance Row X36 = X1
type instance Row X37 = X1
type instance Row X38 = X1
type instance Row X39 = X1
type instance Row X40 = X1
type instance Row X41 = X1
type instance Row X42 = X1
type instance Row X43 = X1
type instance Row X44 = X1
type instance Row X45 = X1
type instance Row X46 = X1
type instance Row X47 = X1
type instance Row X48 = X1
type instance Row X49 = X1
type instance Row X50 = X1
type instance Row X51 = X1
type instance Row X52 = X1
type instance Row X53 = X1
type instance Row X54 = X1
type instance Row X55 = X1
type instance Row X56 = X1
type instance Row X57 = X1
type instance Row X58 = X1
type instance Row X59 = X1
type instance Row X60 = X1
type instance Row X61 = X1
type instance Row X62 = X1
type instance Row X63 = X1
type instance Row X64 = X1
type instance Row X65 = X1
type instance Row X66 = X1
type instance Row X67 = X1
type instance Row X68 = X1
type instance Row X69 = X1
type instance Row X70 = X1
type instance Row X71 = X1
type instance Row X72 = X1
type instance Row X73 = X1
type instance Row X74 = X1
type instance Row X75 = X1
type instance Row X76 = X1
type instance Row X77 = X1
type instance Row X78 = X1
type instance Row X79 = X1
type instance Row X80 = X1
type instance Row X81 = X1
type instance Row X82 = X1
type instance Row X83 = X1
type instance Row X84 = X1
type instance Row X85 = X1
type instance Row X86 = X1
type instance Row X87 = X1
type instance Row X88 = X1
type instance Row X89 = X1
type instance Row X90 = X1
type instance Row X91 = X1
type instance Row X92 = X1
type instance Row X93 = X1
type instance Row X94 = X1
type instance Row X95 = X1
type instance Row X96 = X1
type instance Row X97 = X1
type instance Row X98 = X1
type instance Row X99 = X1
type instance Row X100 = X1
type instance Row X101 = X1
type instance Row X102 = X1
type instance Row X103 = X1
type instance Row X104 = X1
type instance Row X105 = X1
type instance Row X106 = X1
type instance Row X107 = X1
type instance Row X108 = X1
type instance Row X109 = X1
type instance Row X110 = X1
type instance Row X111 = X1
type instance Row X112 = X1
type instance Row X113 = X1
type instance Row X114 = X1
type instance Row X115 = X1
type instance Row X116 = X1
type instance Row X117 = X1
type instance Row X118 = X1
type instance Row X119 = X1
type instance Row X120 = X1
type instance Row X121 = X1
type instance Row X122 = X1
type instance Row X123 = X1
type instance Row X124 = X1
type instance Row X125 = X1
type instance Row X126 = X1
type instance Row X127 = X1
type instance Row X128 = X1
type instance Row X129 = X1
type instance Row X130 = X1
type instance Row X131 = X1
type instance Row X132 = X1
type instance Row X133 = X1
type instance Row X134 = X1
type instance Row X135 = X1
type instance Row X136 = X1
type instance Row X137 = X1
type instance Row X138 = X1
type instance Row X139 = X1
type instance Row X140 = X1
type instance Row X141 = X1
type instance Row X142 = X1
type instance Row X143 = X1
type instance Row X144 = X1
type instance Row X145 = X1
type instance Row X146 = X1
type instance Row X147 = X1
type instance Row X148 = X1
type instance Row X149 = X1
type instance Row X150 = X1
type instance Row X151 = X1
type instance Row X152 = X1
type instance Row X153 = X1
type instance Row X154 = X1
type instance Row X155 = X1
type instance Row X156 = X1
type instance Row X157 = X1
type instance Row X158 = X1
type instance Row X159 = X1
type instance Row X160 = X1
type instance Row X161 = X1
type instance Row X162 = X1
type instance Row X163 = X1
type instance Row X164 = X1
type instance Row X165 = X1
type instance Row X166 = X1
type instance Row X167 = X1
type instance Row X168 = X1
type instance Row X169 = X1
type instance Row X170 = X1
type instance Row X171 = X1
type instance Row X172 = X1
type instance Row X173 = X1
type instance Row X174 = X1
type instance Row X175 = X1
type instance Row X176 = X1
type instance Row X177 = X1
type instance Row X178 = X1
type instance Row X179 = X1
type instance Row X180 = X1
type instance Row X181 = X1
type instance Row X182 = X1
type instance Row X183 = X1
type instance Row X184 = X1
type instance Row X185 = X1
type instance Row X186 = X1
type instance Row X187 = X1
type instance Row X188 = X1
type instance Row X189 = X1
type instance Row X190 = X1
type instance Row X191 = X1
type instance Row X192 = X1
type instance Row X193 = X1
type instance Row X194 = X1
type instance Row X195 = X1
type instance Row X196 = X1
type instance Row X197 = X1
type instance Row X198 = X1
type instance Row X199 = X1
type instance Row X200 = X1
type instance Row X201 = X1
type instance Row X202 = X1
type instance Row X203 = X1
type instance Row X204 = X1
type instance Row X205 = X1
type instance Row X206 = X1
type instance Row X207 = X1
type instance Row X208 = X1
type instance Row X209 = X1
type instance Row X210 = X1
type instance Row X211 = X1
type instance Row X212 = X1
type instance Row X213 = X1
type instance Row X214 = X1
type instance Row X215 = X1
type instance Row X216 = X1
type instance Row X217 = X1
type instance Row X218 = X1
type instance Row X219 = X1
type instance Row X220 = X1
type instance Row X221 = X1
type instance Row X222 = X1
type instance Row X223 = X1
type instance Row X224 = X1
type instance Row X225 = X1
type instance Row X226 = X1
type instance Row X227 = X1
type instance Row X228 = X1
type instance Row X229 = X1
type instance Row X230 = X1
type instance Row X231 = X1
type instance Row X232 = X1
type instance Row X233 = X1
type instance Row X234 = X1
type instance Row X235 = X1
type instance Row X236 = X1
type instance Row X237 = X1
type instance Row X238 = X1
type instance Row X239 = X1
type instance Row X240 = X1
type instance Row X241 = X1
type instance Row X242 = X1
type instance Row X243 = X1
type instance Row X244 = X1
type instance Row X245 = X1
type instance Row X246 = X1
type instance Row X247 = X1
type instance Row X248 = X1
type instance Row X249 = X1
type instance Row X250 = X1
type instance Row X251 = X1
type instance Row X252 = X1
type instance Row X253 = X1
type instance Row X254 = X1
type instance Row X255 = X1
type instance Row X256 = X1

type instance Column X0 = X0
type instance Column X1 = X1
type instance Column X2 = X2
type instance Column X3 = X3
type instance Column X4 = X4
type instance Column X5 = X5
type instance Column X6 = X6
type instance Column X7 = X7
type instance Column X8 = X8
type instance Column X9 = X9
type instance Column X10 = X10
type instance Column X11 = X11
type instance Column X12 = X12
type instance Column X13 = X13
type instance Column X14 = X14
type instance Column X15 = X15
type instance Column X16 = X16
type instance Column X17 = X17
type instance Column X18 = X18
type instance Column X19 = X19
type instance Column X20 = X20
type instance Column X21 = X21
type instance Column X22 = X22
type instance Column X23 = X23
type instance Column X24 = X24
type instance Column X25 = X25
type instance Column X26 = X26
type instance Column X27 = X27
type instance Column X28 = X28
type instance Column X29 = X29
type instance Column X30 = X30
type instance Column X31 = X31
type instance Column X32 = X32
type instance Column X33 = X33
type instance Column X34 = X34
type instance Column X35 = X35
type instance Column X36 = X36
type instance Column X37 = X37
type instance Column X38 = X38
type instance Column X39 = X39
type instance Column X40 = X40
type instance Column X41 = X41
type instance Column X42 = X42
type instance Column X43 = X43
type instance Column X44 = X44
type instance Column X45 = X45
type instance Column X46 = X46
type instance Column X47 = X47
type instance Column X48 = X48
type instance Column X49 = X49
type instance Column X50 = X50
type instance Column X51 = X51
type instance Column X52 = X52
type instance Column X53 = X53
type instance Column X54 = X54
type instance Column X55 = X55
type instance Column X56 = X56
type instance Column X57 = X57
type instance Column X58 = X58
type instance Column X59 = X59
type instance Column X60 = X60
type instance Column X61 = X61
type instance Column X62 = X62
type instance Column X63 = X63
type instance Column X64 = X64
type instance Column X65 = X65
type instance Column X66 = X66
type instance Column X67 = X67
type instance Column X68 = X68
type instance Column X69 = X69
type instance Column X70 = X70
type instance Column X71 = X71
type instance Column X72 = X72
type instance Column X73 = X73
type instance Column X74 = X74
type instance Column X75 = X75
type instance Column X76 = X76
type instance Column X77 = X77
type instance Column X78 = X78
type instance Column X79 = X79
type instance Column X80 = X80
type instance Column X81 = X81
type instance Column X82 = X82
type instance Column X83 = X83
type instance Column X84 = X84
type instance Column X85 = X85
type instance Column X86 = X86
type instance Column X87 = X87
type instance Column X88 = X88
type instance Column X89 = X89
type instance Column X90 = X90
type instance Column X91 = X91
type instance Column X92 = X92
type instance Column X93 = X93
type instance Column X94 = X94
type instance Column X95 = X95
type instance Column X96 = X96
type instance Column X97 = X97
type instance Column X98 = X98
type instance Column X99 = X99
type instance Column X100 = X100
type instance Column X101 = X101
type instance Column X102 = X102
type instance Column X103 = X103
type instance Column X104 = X104
type instance Column X105 = X105
type instance Column X106 = X106
type instance Column X107 = X107
type instance Column X108 = X108
type instance Column X109 = X109
type instance Column X110 = X110
type instance Column X111 = X111
type instance Column X112 = X112
type instance Column X113 = X113
type instance Column X114 = X114
type instance Column X115 = X115
type instance Column X116 = X116
type instance Column X117 = X117
type instance Column X118 = X118
type instance Column X119 = X119
type instance Column X120 = X120
type instance Column X121 = X121
type instance Column X122 = X122
type instance Column X123 = X123
type instance Column X124 = X124
type instance Column X125 = X125
type instance Column X126 = X126
type instance Column X127 = X127
type instance Column X128 = X128
type instance Column X129 = X129
type instance Column X130 = X130
type instance Column X131 = X131
type instance Column X132 = X132
type instance Column X133 = X133
type instance Column X134 = X134
type instance Column X135 = X135
type instance Column X136 = X136
type instance Column X137 = X137
type instance Column X138 = X138
type instance Column X139 = X139
type instance Column X140 = X140
type instance Column X141 = X141
type instance Column X142 = X142
type instance Column X143 = X143
type instance Column X144 = X144
type instance Column X145 = X145
type instance Column X146 = X146
type instance Column X147 = X147
type instance Column X148 = X148
type instance Column X149 = X149
type instance Column X150 = X150
type instance Column X151 = X151
type instance Column X152 = X152
type instance Column X153 = X153
type instance Column X154 = X154
type instance Column X155 = X155
type instance Column X156 = X156
type instance Column X157 = X157
type instance Column X158 = X158
type instance Column X159 = X159
type instance Column X160 = X160
type instance Column X161 = X161
type instance Column X162 = X162
type instance Column X163 = X163
type instance Column X164 = X164
type instance Column X165 = X165
type instance Column X166 = X166
type instance Column X167 = X167
type instance Column X168 = X168
type instance Column X169 = X169
type instance Column X170 = X170
type instance Column X171 = X171
type instance Column X172 = X172
type instance Column X173 = X173
type instance Column X174 = X174
type instance Column X175 = X175
type instance Column X176 = X176
type instance Column X177 = X177
type instance Column X178 = X178
type instance Column X179 = X179
type instance Column X180 = X180
type instance Column X181 = X181
type instance Column X182 = X182
type instance Column X183 = X183
type instance Column X184 = X184
type instance Column X185 = X185
type instance Column X186 = X186
type instance Column X187 = X187
type instance Column X188 = X188
type instance Column X189 = X189
type instance Column X190 = X190
type instance Column X191 = X191
type instance Column X192 = X192
type instance Column X193 = X193
type instance Column X194 = X194
type instance Column X195 = X195
type instance Column X196 = X196
type instance Column X197 = X197
type instance Column X198 = X198
type instance Column X199 = X199
type instance Column X200 = X200
type instance Column X201 = X201
type instance Column X202 = X202
type instance Column X203 = X203
type instance Column X204 = X204
type instance Column X205 = X205
type instance Column X206 = X206
type instance Column X207 = X207
type instance Column X208 = X208
type instance Column X209 = X209
type instance Column X210 = X210
type instance Column X211 = X211
type instance Column X212 = X212
type instance Column X213 = X213
type instance Column X214 = X214
type instance Column X215 = X215
type instance Column X216 = X216
type instance Column X217 = X217
type instance Column X218 = X218
type instance Column X219 = X219
type instance Column X220 = X220
type instance Column X221 = X221
type instance Column X222 = X222
type instance Column X223 = X223
type instance Column X224 = X224
type instance Column X225 = X225
type instance Column X226 = X226
type instance Column X227 = X227
type instance Column X228 = X228
type instance Column X229 = X229
type instance Column X230 = X230
type instance Column X231 = X231
type instance Column X232 = X232
type instance Column X233 = X233
type instance Column X234 = X234
type instance Column X235 = X235
type instance Column X236 = X236
type instance Column X237 = X237
type instance Column X238 = X238
type instance Column X239 = X239
type instance Column X240 = X240
type instance Column X241 = X241
type instance Column X242 = X242
type instance Column X243 = X243
type instance Column X244 = X244
type instance Column X245 = X245
type instance Column X246 = X246
type instance Column X247 = X247
type instance Column X248 = X248
type instance Column X249 = X249
type instance Column X250 = X250
type instance Column X251 = X251
type instance Column X252 = X252
type instance Column X253 = X253
type instance Column X254 = X254
type instance Column X255 = X255
type instance Column X256 = X256

