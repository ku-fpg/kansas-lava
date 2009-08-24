{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Data.Sized.Ix 
	( X1
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
	, module Data.Sized.Size
	) where
	
import Language.Haskell.TH
import Data.Sized.Ix.TH
import Data.Ix
import Data.Sized.Size

$(sizedTypeGenForUpto 256)
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


