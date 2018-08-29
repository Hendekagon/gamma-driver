(ns gamma-driver.gl-enums
  #?(:cljs (:require-macros [gamma-driver.gl-enums :refer [defall]])))

(def gl1-enums
{
  :gl/DEPTH_BUFFER_BIT                0x00000100
  :gl/STENCIL_BUFFER_BIT              0x00000400
  :gl/COLOR_BUFFER_BIT                0x00004000

  :gl/POINTS                          0x0000
  :gl/LINES                           0x0001
  :gl/LINE_LOOP                       0x0002
  :gl/LINE_STRIP                      0x0003
  :gl/TRIANGLES                       0x0004
  :gl/TRIANGLE_STRIP                  0x0005
  :gl/TRIANGLE_FAN                    0x0006

  :gl/ZERO                            0
  :gl/ONE                             1
  :gl/SRC_COLOR                       0x0300
  :gl/ONE_MINUS_SRC_COLOR             0x0301
  :gl/SRC_ALPHA                       0x0302
  :gl/ONE_MINUS_SRC_ALPHA             0x0303
  :gl/DST_ALPHA                       0x0304
  :gl/ONE_MINUS_DST_ALPHA             0x0305


  :gl/DST_COLOR                       0x0306
  :gl/ONE_MINUS_DST_COLOR             0x0307
  :gl/SRC_ALPHA_SATURATE              0x0308

  :gl/FUNC_ADD                        0x8006
  :gl/BLEND_EQUATION                  0x8009
  :gl/BLEND_EQUATION_RGB              0x8009
  :gl/BLEND_EQUATION_ALPHA            0x883D

  :gl/FUNC_SUBTRACT                   0x800A
  :gl/FUNC_REVERSE_SUBTRACT           0x800B

  :gl/BLEND_DST_RGB                   0x80C8
  :gl/BLEND_SRC_RGB                   0x80C9
  :gl/BLEND_DST_ALPHA                 0x80CA
  :gl/BLEND_SRC_ALPHA                 0x80CB
  :gl/CONSTANT_COLOR                  0x8001
  :gl/ONE_MINUS_CONSTANT_COLOR        0x8002
  :gl/CONSTANT_ALPHA                  0x8003
  :gl/ONE_MINUS_CONSTANT_ALPHA        0x8004
  :gl/BLEND_COLOR                     0x8005

  :gl/ARRAY_BUFFER                    0x8892
  :gl/ELEMENT_ARRAY_BUFFER            0x8893
  :gl/ARRAY_BUFFER_BINDING            0x8894
  :gl/ELEMENT_ARRAY_BUFFER_BINDING    0x8895

  :gl/STREAM_DRAW                     0x88E0
  :gl/STATIC_DRAW                     0x88E4
  :gl/DYNAMIC_DRAW                    0x88E8

  :gl/BUFFER_SIZE                     0x8764
  :gl/BUFFER_USAGE                    0x8765

  :gl/CURRENT_VERTEX_ATTRIB           0x8626

  :gl/FRONT                           0x0404
  :gl/BACK                            0x0405
  :gl/FRONT_AND_BACK                  0x0408

  :gl/CULL_FACE                       0x0B44
  :gl/BLEND                           0x0BE2
  :gl/DITHER                          0x0BD0
  :gl/STENCIL_TEST                    0x0B90
  :gl/DEPTH_TEST                      0x0B71
  :gl/SCISSOR_TEST                    0x0C11
  :gl/POLYGON_OFFSET_FILL             0x8037
  :gl/SAMPLE_ALPHA_TO_COVERAGE        0x809E
  :gl/SAMPLE_COVERAGE                 0x80A0

  :gl/NO_ERROR                        0
  :gl/INVALID_ENUM                    0x0500
  :gl/INVALID_VALUE                   0x0501
  :gl/INVALID_OPERATION               0x0502
  :gl/OUT_OF_MEMORY                   0x0505

  :gl/CW                              0x0900
  :gl/CCW                             0x0901

  :gl/LINE_WIDTH                      0x0B21
  :gl/ALIASED_POINT_SIZE_RANGE        0x846D
  :gl/ALIASED_LINE_WIDTH_RANGE        0x846E
  :gl/CULL_FACE_MODE                  0x0B45
  :gl/FRONT_FACE                      0x0B46
  :gl/DEPTH_RANGE                     0x0B70
  :gl/DEPTH_WRITEMASK                 0x0B72
  :gl/DEPTH_CLEAR_VALUE               0x0B73
  :gl/DEPTH_FUNC                      0x0B74
  :gl/STENCIL_CLEAR_VALUE             0x0B91
  :gl/STENCIL_FUNC                    0x0B92
  :gl/STENCIL_FAIL                    0x0B94
  :gl/STENCIL_PASS_DEPTH_FAIL         0x0B95
  :gl/STENCIL_PASS_DEPTH_PASS         0x0B96
  :gl/STENCIL_REF                     0x0B97
  :gl/STENCIL_VALUE_MASK              0x0B93
  :gl/STENCIL_WRITEMASK               0x0B98
  :gl/STENCIL_BACK_FUNC               0x8800
  :gl/STENCIL_BACK_FAIL               0x8801
  :gl/STENCIL_BACK_PASS_DEPTH_FAIL    0x8802
  :gl/STENCIL_BACK_PASS_DEPTH_PASS    0x8803
  :gl/STENCIL_BACK_REF                0x8CA3
  :gl/STENCIL_BACK_VALUE_MASK         0x8CA4
  :gl/STENCIL_BACK_WRITEMASK          0x8CA5
  :gl/VIEWPORT                        0x0BA2
  :gl/SCISSOR_BOX                     0x0C10

  :gl/COLOR_CLEAR_VALUE               0x0C22
  :gl/COLOR_WRITEMASK                 0x0C23
  :gl/UNPACK_ALIGNMENT                0x0CF5
  :gl/PACK_ALIGNMENT                  0x0D05
  :gl/MAX_TEXTURE_SIZE                0x0D33
  :gl/MAX_VIEWPORT_DIMS               0x0D3A
  :gl/SUBPIXEL_BITS                   0x0D50
  :gl/RED_BITS                        0x0D52
  :gl/GREEN_BITS                      0x0D53
  :gl/BLUE_BITS                       0x0D54
  :gl/ALPHA_BITS                      0x0D55
  :gl/DEPTH_BITS                      0x0D56
  :gl/STENCIL_BITS                    0x0D57
  :gl/POLYGON_OFFSET_UNITS            0x2A00

  :gl/POLYGON_OFFSET_FACTOR           0x8038
  :gl/TEXTURE_BINDING_2D              0x8069
  :gl/SAMPLE_BUFFERS                  0x80A8
  :gl/SAMPLES                         0x80A9
  :gl/SAMPLE_COVERAGE_VALUE           0x80AA
  :gl/SAMPLE_COVERAGE_INVERT          0x80AB


  :gl/COMPRESSED_TEXTURE_FORMATS      0x86A3

  :gl/DONT_CARE                       0x1100
  :gl/FASTEST                         0x1101
  :gl/NICEST                          0x1102

  :gl/GENERATE_MIPMAP_HINT             0x8192

  :gl/BYTE                            0x1400
  :gl/UNSIGNED_BYTE                   0x1401
  :gl/SHORT                           0x1402
  :gl/UNSIGNED_SHORT                  0x1403
  :gl/INT                             0x1404
  :gl/UNSIGNED_INT                    0x1405
  :gl/FLOAT                           0x1406

  :gl/DEPTH_COMPONENT                 0x1902
  :gl/ALPHA                           0x1906
  :gl/RGB                             0x1907
  :gl/RGBA                            0x1908
  :gl/LUMINANCE                       0x1909
  :gl/LUMINANCE_ALPHA                 0x190A

  :gl/UNSIGNED_SHORT_4_4_4_4          0x8033
  :gl/UNSIGNED_SHORT_5_5_5_1          0x8034
  :gl/UNSIGNED_SHORT_5_6_5            0x8363

  :gl/FRAGMENT_SHADER                   0x8B30
  :gl/VERTEX_SHADER                     0x8B31
  :gl/MAX_VERTEX_ATTRIBS                0x8869
  :gl/MAX_VERTEX_UNIFORM_VECTORS        0x8DFB
  :gl/MAX_VARYING_VECTORS               0x8DFC
  :gl/MAX_COMBINED_TEXTURE_IMAGE_UNITS  0x8B4D
  :gl/MAX_VERTEX_TEXTURE_IMAGE_UNITS    0x8B4C
  :gl/MAX_TEXTURE_IMAGE_UNITS           0x8872
  :gl/MAX_FRAGMENT_UNIFORM_VECTORS      0x8DFD
  :gl/SHADER_TYPE                       0x8B4F
  :gl/DELETE_STATUS                     0x8B80
  :gl/LINK_STATUS                       0x8B82
  :gl/VALIDATE_STATUS                   0x8B83
  :gl/ATTACHED_SHADERS                  0x8B85
  :gl/ACTIVE_UNIFORMS                   0x8B86
  :gl/ACTIVE_ATTRIBUTES                 0x8B89
  :gl/SHADING_LANGUAGE_VERSION          0x8B8C
  :gl/CURRENT_PROGRAM                   0x8B8D

  :gl/NEVER                           0x0200
  :gl/LESS                            0x0201
  :gl/EQUAL                           0x0202
  :gl/LEQUAL                          0x0203
  :gl/GREATER                         0x0204
  :gl/NOTEQUAL                        0x0205
  :gl/GEQUAL                          0x0206
  :gl/ALWAYS                          0x0207

  :gl/KEEP                            0x1E00
  :gl/REPLACE                         0x1E01
  :gl/INCR                            0x1E02
  :gl/DECR                            0x1E03
  :gl/INVERT                          0x150A
  :gl/INCR_WRAP                       0x8507
  :gl/DECR_WRAP                       0x8508

  :gl/VENDOR                          0x1F00
  :gl/RENDERER                        0x1F01
  :gl/VERSION                         0x1F02

  :gl/NEAREST                         0x2600
  :gl/LINEAR                          0x2601

  :gl/NEAREST_MIPMAP_NEAREST          0x2700
  :gl/LINEAR_MIPMAP_NEAREST           0x2701
  :gl/NEAREST_MIPMAP_LINEAR           0x2702
  :gl/LINEAR_MIPMAP_LINEAR            0x2703

  :gl/TEXTURE_MAG_FILTER              0x2800
  :gl/TEXTURE_MIN_FILTER              0x2801
  :gl/TEXTURE_WRAP_S                  0x2802
  :gl/TEXTURE_WRAP_T                  0x2803

  :gl/TEXTURE_2D                      0x0DE1
  :gl/TEXTURE                         0x1702

  :gl/TEXTURE_CUBE_MAP                0x8513
  :gl/TEXTURE_BINDING_CUBE_MAP        0x8514
  :gl/TEXTURE_CUBE_MAP_POSITIVE_X     0x8515
  :gl/TEXTURE_CUBE_MAP_NEGATIVE_X     0x8516
  :gl/TEXTURE_CUBE_MAP_POSITIVE_Y     0x8517
  :gl/TEXTURE_CUBE_MAP_NEGATIVE_Y     0x8518
  :gl/TEXTURE_CUBE_MAP_POSITIVE_Z     0x8519
  :gl/TEXTURE_CUBE_MAP_NEGATIVE_Z     0x851A
  :gl/MAX_CUBE_MAP_TEXTURE_SIZE       0x851C

  :gl/TEXTURE0                        0x84C0
  :gl/TEXTURE1                        0x84C1
  :gl/TEXTURE2                        0x84C2
  :gl/TEXTURE3                        0x84C3
  :gl/TEXTURE4                        0x84C4
  :gl/TEXTURE5                        0x84C5
  :gl/TEXTURE6                        0x84C6
  :gl/TEXTURE7                        0x84C7
  :gl/TEXTURE8                        0x84C8
  :gl/TEXTURE9                        0x84C9
  :gl/TEXTURE10                       0x84CA
  :gl/TEXTURE11                       0x84CB
  :gl/TEXTURE12                       0x84CC
  :gl/TEXTURE13                       0x84CD
  :gl/TEXTURE14                       0x84CE
  :gl/TEXTURE15                       0x84CF
  :gl/TEXTURE16                       0x84D0
  :gl/TEXTURE17                       0x84D1
  :gl/TEXTURE18                       0x84D2
  :gl/TEXTURE19                       0x84D3
  :gl/TEXTURE20                       0x84D4
  :gl/TEXTURE21                       0x84D5
  :gl/TEXTURE22                       0x84D6
  :gl/TEXTURE23                       0x84D7
  :gl/TEXTURE24                       0x84D8
  :gl/TEXTURE25                       0x84D9
  :gl/TEXTURE26                       0x84DA
  :gl/TEXTURE27                       0x84DB
  :gl/TEXTURE28                       0x84DC
  :gl/TEXTURE29                       0x84DD
  :gl/TEXTURE30                       0x84DE
  :gl/TEXTURE31                       0x84DF
  :gl/ACTIVE_TEXTURE                  0x84E0

  :gl/REPEAT                          0x2901
  :gl/CLAMP_TO_EDGE                   0x812F
  :gl/MIRRORED_REPEAT                 0x8370

  :gl/FLOAT_VEC2                      0x8B50
  :gl/FLOAT_VEC3                      0x8B51
  :gl/FLOAT_VEC4                      0x8B52
  :gl/INT_VEC2                        0x8B53
  :gl/INT_VEC3                        0x8B54
  :gl/INT_VEC4                        0x8B55
  :gl/BOOL                            0x8B56
  :gl/BOOL_VEC2                       0x8B57
  :gl/BOOL_VEC3                       0x8B58
  :gl/BOOL_VEC4                       0x8B59
  :gl/FLOAT_MAT2                      0x8B5A
  :gl/FLOAT_MAT3                      0x8B5B
  :gl/FLOAT_MAT4                      0x8B5C
  :gl/SAMPLER_2D                      0x8B5E
  :gl/SAMPLER_CUBE                    0x8B60

  :gl/VERTEX_ATTRIB_ARRAY_ENABLED         0x8622
  :gl/VERTEX_ATTRIB_ARRAY_SIZE            0x8623
  :gl/VERTEX_ATTRIB_ARRAY_STRIDE          0x8624
  :gl/VERTEX_ATTRIB_ARRAY_TYPE            0x8625
  :gl/VERTEX_ATTRIB_ARRAY_NORMALIZED      0x886A
  :gl/VERTEX_ATTRIB_ARRAY_POINTER         0x8645
  :gl/VERTEX_ATTRIB_ARRAY_BUFFER_BINDING  0x889F

  :gl/IMPLEMENTATION_COLOR_READ_TYPE    0x8B9A
  :gl/IMPLEMENTATION_COLOR_READ_FORMAT  0x8B9B

  :gl/COMPILE_STATUS                  0x8B81

  :gl/LOW_FLOAT                       0x8DF0
  :gl/MEDIUM_FLOAT                    0x8DF1
  :gl/HIGH_FLOAT                      0x8DF2
  :gl/LOW_INT                         0x8DF3
  :gl/MEDIUM_INT                      0x8DF4
  :gl/HIGH_INT                        0x8DF5

  :gl/FRAMEBUFFER                     0x8D40
  :gl/RENDERBUFFER                    0x8D41

  :gl/RGBA4                           0x8056
  :gl/RGB5_A1                         0x8057
  :gl/RGB565                          0x8D62
  :gl/DEPTH_COMPONENT16               0x81A5
  :gl/STENCIL_INDEX8                  0x8D48
  :gl/DEPTH_STENCIL                   0x84F9

  :gl/RENDERBUFFER_WIDTH              0x8D42
  :gl/RENDERBUFFER_HEIGHT             0x8D43
  :gl/RENDERBUFFER_INTERNAL_FORMAT    0x8D44
  :gl/RENDERBUFFER_RED_SIZE           0x8D50
  :gl/RENDERBUFFER_GREEN_SIZE         0x8D51
  :gl/RENDERBUFFER_BLUE_SIZE          0x8D52
  :gl/RENDERBUFFER_ALPHA_SIZE         0x8D53
  :gl/RENDERBUFFER_DEPTH_SIZE         0x8D54
  :gl/RENDERBUFFER_STENCIL_SIZE       0x8D55

  :gl/FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE            0x8CD0
  :gl/FRAMEBUFFER_ATTACHMENT_OBJECT_NAME            0x8CD1
  :gl/FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL          0x8CD2
  :gl/FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE  0x8CD3

  :gl/COLOR_ATTACHMENT0               0x8CE0
  :gl/DEPTH_ATTACHMENT                0x8D00
  :gl/STENCIL_ATTACHMENT              0x8D20
  :gl/DEPTH_STENCIL_ATTACHMENT        0x821A

  :gl/NONE                            0

  :gl/FRAMEBUFFER_COMPLETE                       0x8CD5
  :gl/FRAMEBUFFER_INCOMPLETE_ATTACHMENT          0x8CD6
  :gl/FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT  0x8CD7
  :gl/FRAMEBUFFER_INCOMPLETE_DIMENSIONS          0x8CD9
  :gl/FRAMEBUFFER_UNSUPPORTED                    0x8CDD

  :gl/FRAMEBUFFER_BINDING             0x8CA6
  :gl/RENDERBUFFER_BINDING            0x8CA7
  :gl/MAX_RENDERBUFFER_SIZE           0x84E8

  :gl/INVALID_FRAMEBUFFER_OPERATION   0x0506

  :gl/UNPACK_FLIP_Y_WEBGL             0x9240
  :gl/UNPACK_PREMULTIPLY_ALPHA_WEBGL  0x9241
  :gl/CONTEXT_LOST_WEBGL              0x9242
  :gl/UNPACK_COLORSPACE_CONVERSION_WEBGL  0x9243
  :gl/BROWSER_DEFAULT_WEBGL           0x9244
})

(def gl2-enums
  {
    :gl/READ_BUFFER                                    0x0C02
    :gl/UNPACK_ROW_LENGTH                              0x0CF2
    :gl/UNPACK_SKIP_ROWS                               0x0CF3
    :gl/UNPACK_SKIP_PIXELS                             0x0CF4
    :gl/PACK_ROW_LENGTH                                0x0D02
    :gl/PACK_SKIP_ROWS                                 0x0D03
    :gl/PACK_SKIP_PIXELS                               0x0D04
    :gl/COLOR                                          0x1800
    :gl/DEPTH                                          0x1801
    :gl/STENCIL                                        0x1802
    :gl/RED                                            0x1903
    :gl/RGB8                                           0x8051
    :gl/RGBA8                                          0x8058
    :gl/RGB10_A2                                       0x8059
    :gl/TEXTURE_BINDING_3D                             0x806A
    :gl/UNPACK_SKIP_IMAGES                             0x806D
    :gl/UNPACK_IMAGE_HEIGHT                            0x806E
    :gl/TEXTURE_3D                                     0x806F
    :gl/TEXTURE_WRAP_R                                 0x8072
    :gl/MAX_3D_TEXTURE_SIZE                            0x8073
    :gl/UNSIGNED_INT_2_10_10_10_REV                    0x8368
    :gl/MAX_ELEMENTS_VERTICES                          0x80E8
    :gl/MAX_ELEMENTS_INDICES                           0x80E9
    :gl/TEXTURE_MIN_LOD                                0x813A
    :gl/TEXTURE_MAX_LOD                                0x813B
    :gl/TEXTURE_BASE_LEVEL                             0x813C
    :gl/TEXTURE_MAX_LEVEL                              0x813D
    :gl/MIN                                            0x8007
    :gl/MAX                                            0x8008
    :gl/DEPTH_COMPONENT24                              0x81A6
    :gl/MAX_TEXTURE_LOD_BIAS                           0x84FD
    :gl/TEXTURE_COMPARE_MODE                           0x884C
    :gl/TEXTURE_COMPARE_FUNC                           0x884D
    :gl/CURRENT_QUERY                                  0x8865
    :gl/QUERY_RESULT                                   0x8866
    :gl/QUERY_RESULT_AVAILABLE                         0x8867
    :gl/STREAM_READ                                    0x88E1
    :gl/STREAM_COPY                                    0x88E2
    :gl/STATIC_READ                                    0x88E5
    :gl/STATIC_COPY                                    0x88E6
    :gl/DYNAMIC_READ                                   0x88E9
    :gl/DYNAMIC_COPY                                   0x88EA
    :gl/MAX_DRAW_BUFFERS                               0x8824
    :gl/DRAW_BUFFER0                                   0x8825
    :gl/DRAW_BUFFER1                                   0x8826
    :gl/DRAW_BUFFER2                                   0x8827
    :gl/DRAW_BUFFER3                                   0x8828
    :gl/DRAW_BUFFER4                                   0x8829
    :gl/DRAW_BUFFER5                                   0x882A
    :gl/DRAW_BUFFER6                                   0x882B
    :gl/DRAW_BUFFER7                                   0x882C
    :gl/DRAW_BUFFER8                                   0x882D
    :gl/DRAW_BUFFER9                                   0x882E
    :gl/DRAW_BUFFER10                                  0x882F
    :gl/DRAW_BUFFER11                                  0x8830
    :gl/DRAW_BUFFER12                                  0x8831
    :gl/DRAW_BUFFER13                                  0x8832
    :gl/DRAW_BUFFER14                                  0x8833
    :gl/DRAW_BUFFER15                                  0x8834
    :gl/MAX_FRAGMENT_UNIFORM_COMPONENTS                0x8B49
    :gl/MAX_VERTEX_UNIFORM_COMPONENTS                  0x8B4A
    :gl/SAMPLER_3D                                     0x8B5F
    :gl/SAMPLER_2D_SHADOW                              0x8B62
    :gl/FRAGMENT_SHADER_DERIVATIVE_HINT                0x8B8B
    :gl/PIXEL_PACK_BUFFER                              0x88EB
    :gl/PIXEL_UNPACK_BUFFER                            0x88EC
    :gl/PIXEL_PACK_BUFFER_BINDING                      0x88ED
    :gl/PIXEL_UNPACK_BUFFER_BINDING                    0x88EF
    :gl/FLOAT_MAT2x3                                   0x8B65
    :gl/FLOAT_MAT2x4                                   0x8B66
    :gl/FLOAT_MAT3x2                                   0x8B67
    :gl/FLOAT_MAT3x4                                   0x8B68
    :gl/FLOAT_MAT4x2                                   0x8B69
    :gl/FLOAT_MAT4x3                                   0x8B6A
    :gl/SRGB                                           0x8C40
    :gl/SRGB8                                          0x8C41
    :gl/SRGB8_ALPHA8                                   0x8C43
    :gl/COMPARE_REF_TO_TEXTURE                         0x884E
    :gl/RGBA32F                                        0x8814
    :gl/RGB32F                                         0x8815
    :gl/RGBA16F                                        0x881A
    :gl/RGB16F                                         0x881B
    :gl/VERTEX_ATTRIB_ARRAY_INTEGER                    0x88FD
    :gl/MAX_ARRAY_TEXTURE_LAYERS                       0x88FF
    :gl/MIN_PROGRAM_TEXEL_OFFSET                       0x8904
    :gl/MAX_PROGRAM_TEXEL_OFFSET                       0x8905
    :gl/MAX_VARYING_COMPONENTS                         0x8B4B
    :gl/TEXTURE_2D_ARRAY                               0x8C1A
    :gl/TEXTURE_BINDING_2D_ARRAY                       0x8C1D
    :gl/R11F_G11F_B10F                                 0x8C3A
    :gl/UNSIGNED_INT_10F_11F_11F_REV                   0x8C3B
    :gl/RGB9_E5                                        0x8C3D
    :gl/UNSIGNED_INT_5_9_9_9_REV                       0x8C3E
    :gl/TRANSFORM_FEEDBACK_BUFFER_MODE                 0x8C7F
    :gl/MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS     0x8C80
    :gl/TRANSFORM_FEEDBACK_VARYINGS                    0x8C83
    :gl/TRANSFORM_FEEDBACK_BUFFER_START                0x8C84
    :gl/TRANSFORM_FEEDBACK_BUFFER_SIZE                 0x8C85
    :gl/TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN          0x8C88
    :gl/RASTERIZER_DISCARD                             0x8C89
    :gl/MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS  0x8C8A
    :gl/MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS        0x8C8B
    :gl/INTERLEAVED_ATTRIBS                            0x8C8C
    :gl/SEPARATE_ATTRIBS                               0x8C8D
    :gl/TRANSFORM_FEEDBACK_BUFFER                      0x8C8E
    :gl/TRANSFORM_FEEDBACK_BUFFER_BINDING              0x8C8F
    :gl/RGBA32UI                                       0x8D70
    :gl/RGB32UI                                        0x8D71
    :gl/RGBA16UI                                       0x8D76
    :gl/RGB16UI                                        0x8D77
    :gl/RGBA8UI                                        0x8D7C
    :gl/RGB8UI                                         0x8D7D
    :gl/RGBA32I                                        0x8D82
    :gl/RGB32I                                         0x8D83
    :gl/RGBA16I                                        0x8D88
    :gl/RGB16I                                         0x8D89
    :gl/RGBA8I                                         0x8D8E
    :gl/RGB8I                                          0x8D8F
    :gl/RED_INTEGER                                    0x8D94
    :gl/RGB_INTEGER                                    0x8D98
    :gl/RGBA_INTEGER                                   0x8D99
    :gl/SAMPLER_2D_ARRAY                               0x8DC1
    :gl/SAMPLER_2D_ARRAY_SHADOW                        0x8DC4
    :gl/SAMPLER_CUBE_SHADOW                            0x8DC5
    :gl/UNSIGNED_INT_VEC2                              0x8DC6
    :gl/UNSIGNED_INT_VEC3                              0x8DC7
    :gl/UNSIGNED_INT_VEC4                              0x8DC8
    :gl/INT_SAMPLER_2D                                 0x8DCA
    :gl/INT_SAMPLER_3D                                 0x8DCB
    :gl/INT_SAMPLER_CUBE                               0x8DCC
    :gl/INT_SAMPLER_2D_ARRAY                           0x8DCF
    :gl/UNSIGNED_INT_SAMPLER_2D                        0x8DD2
    :gl/UNSIGNED_INT_SAMPLER_3D                        0x8DD3
    :gl/UNSIGNED_INT_SAMPLER_CUBE                      0x8DD4
    :gl/UNSIGNED_INT_SAMPLER_2D_ARRAY                  0x8DD7
    :gl/DEPTH_COMPONENT32F                             0x8CAC
    :gl/DEPTH32F_STENCIL8                              0x8CAD
    :gl/FLOAT_32_UNSIGNED_INT_24_8_REV                 0x8DAD
    :gl/FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING          0x8210
    :gl/FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE          0x8211
    :gl/FRAMEBUFFER_ATTACHMENT_RED_SIZE                0x8212
    :gl/FRAMEBUFFER_ATTACHMENT_GREEN_SIZE              0x8213
    :gl/FRAMEBUFFER_ATTACHMENT_BLUE_SIZE               0x8214
    :gl/FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE              0x8215
    :gl/FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE              0x8216
    :gl/FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE            0x8217
    :gl/FRAMEBUFFER_DEFAULT                            0x8218
    :gl/DEPTH_STENCIL_ATTACHMENT                       0x821A
    :gl/DEPTH_STENCIL                                  0x84F9
    :gl/UNSIGNED_INT_24_8                              0x84FA
    :gl/DEPTH24_STENCIL8                               0x88F0
    :gl/UNSIGNED_NORMALIZED                            0x8C17
    :gl/DRAW_FRAMEBUFFER_BINDING                       0x8CA6
    :gl/READ_FRAMEBUFFER                               0x8CA8
    :gl/DRAW_FRAMEBUFFER                               0x8CA9
    :gl/READ_FRAMEBUFFER_BINDING                       0x8CAA
    :gl/RENDERBUFFER_SAMPLES                           0x8CAB
    :gl/FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER           0x8CD4
    :gl/MAX_COLOR_ATTACHMENTS                          0x8CDF
    :gl/COLOR_ATTACHMENT1                              0x8CE1
    :gl/COLOR_ATTACHMENT2                              0x8CE2
    :gl/COLOR_ATTACHMENT3                              0x8CE3
    :gl/COLOR_ATTACHMENT4                              0x8CE4
    :gl/COLOR_ATTACHMENT5                              0x8CE5
    :gl/COLOR_ATTACHMENT6                              0x8CE6
    :gl/COLOR_ATTACHMENT7                              0x8CE7
    :gl/COLOR_ATTACHMENT8                              0x8CE8
    :gl/COLOR_ATTACHMENT9                              0x8CE9
    :gl/COLOR_ATTACHMENT10                             0x8CEA
    :gl/COLOR_ATTACHMENT11                             0x8CEB
    :gl/COLOR_ATTACHMENT12                             0x8CEC
    :gl/COLOR_ATTACHMENT13                             0x8CED
    :gl/COLOR_ATTACHMENT14                             0x8CEE
    :gl/COLOR_ATTACHMENT15                             0x8CEF
    :gl/FRAMEBUFFER_INCOMPLETE_MULTISAMPLE             0x8D56
    :gl/MAX_SAMPLES                                    0x8D57
    :gl/HALF_FLOAT                                     0x140B
    :gl/RG                                             0x8227
    :gl/RG_INTEGER                                     0x8228
    :gl/R8                                             0x8229
    :gl/RG8                                            0x822B
    :gl/R16F                                           0x822D
    :gl/R32F                                           0x822E
    :gl/RG16F                                          0x822F
    :gl/RG32F                                          0x8230
    :gl/R8I                                            0x8231
    :gl/R8UI                                           0x8232
    :gl/R16I                                           0x8233
    :gl/R16UI                                          0x8234
    :gl/R32I                                           0x8235
    :gl/R32UI                                          0x8236
    :gl/RG8I                                           0x8237
    :gl/RG8UI                                          0x8238
    :gl/RG16I                                          0x8239
    :gl/RG16UI                                         0x823A
    :gl/RG32I                                          0x823B
    :gl/RG32UI                                         0x823C
    :gl/VERTEX_ARRAY_BINDING                           0x85B5
    :gl/R8_SNORM                                       0x8F94
    :gl/RG8_SNORM                                      0x8F95
    :gl/RGB8_SNORM                                     0x8F96
    :gl/RGBA8_SNORM                                    0x8F97
    :gl/SIGNED_NORMALIZED                              0x8F9C
    :gl/COPY_READ_BUFFER                               0x8F36
    :gl/COPY_WRITE_BUFFER                              0x8F37
    :gl/COPY_READ_BUFFER_BINDING                       0x8F36
    :gl/COPY_WRITE_BUFFER_BINDING                      0x8F37
    :gl/UNIFORM_BUFFER                                 0x8A11
    :gl/UNIFORM_BUFFER_BINDING                         0x8A28
    :gl/UNIFORM_BUFFER_START                           0x8A29
    :gl/UNIFORM_BUFFER_SIZE                            0x8A2A
    :gl/MAX_VERTEX_UNIFORM_BLOCKS                      0x8A2B
    :gl/MAX_FRAGMENT_UNIFORM_BLOCKS                    0x8A2D
    :gl/MAX_COMBINED_UNIFORM_BLOCKS                    0x8A2E
    :gl/MAX_UNIFORM_BUFFER_BINDINGS                    0x8A2F
    :gl/MAX_UNIFORM_BLOCK_SIZE                         0x8A30
    :gl/MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS         0x8A31
    :gl/MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS       0x8A33
    :gl/UNIFORM_BUFFER_OFFSET_ALIGNMENT                0x8A34
    :gl/ACTIVE_UNIFORM_BLOCKS                          0x8A36
    :gl/UNIFORM_TYPE                                   0x8A37
    :gl/UNIFORM_SIZE                                   0x8A38
    :gl/UNIFORM_BLOCK_INDEX                            0x8A3A
    :gl/UNIFORM_OFFSET                                 0x8A3B
    :gl/UNIFORM_ARRAY_STRIDE                           0x8A3C
    :gl/UNIFORM_MATRIX_STRIDE                          0x8A3D
    :gl/UNIFORM_IS_ROW_MAJOR                           0x8A3E
    :gl/UNIFORM_BLOCK_BINDING                          0x8A3F
    :gl/UNIFORM_BLOCK_DATA_SIZE                        0x8A40
    :gl/UNIFORM_BLOCK_ACTIVE_UNIFORMS                  0x8A42
    :gl/UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES           0x8A43
    :gl/UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER      0x8A44
    :gl/UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER    0x8A46
    :gl/INVALID_INDEX                                  0xFFFFFFFF
    :gl/MAX_VERTEX_OUTPUT_COMPONENTS                   0x9122
    :gl/MAX_FRAGMENT_INPUT_COMPONENTS                  0x9125
    :gl/MAX_SERVER_WAIT_TIMEOUT                        0x9111
    :gl/OBJECT_TYPE                                    0x9112
    :gl/SYNC_CONDITION                                 0x9113
    :gl/SYNC_STATUS                                    0x9114
    :gl/SYNC_FLAGS                                     0x9115
    :gl/SYNC_FENCE                                     0x9116
    :gl/SYNC_GPU_COMMANDS_COMPLETE                     0x9117
    :gl/UNSIGNALED                                     0x9118
    :gl/SIGNALED                                       0x9119
    :gl/ALREADY_SIGNALED                               0x911A
    :gl/TIMEOUT_EXPIRED                                0x911B
    :gl/CONDITION_SATISFIED                            0x911C
    :gl/WAIT_FAILED                                    0x911D
    :gl/SYNC_FLUSH_COMMANDS_BIT                        0x00000001
    :gl/VERTEX_ATTRIB_ARRAY_DIVISOR                    0x88FE
    :gl/ANY_SAMPLES_PASSED                             0x8C2F
    :gl/ANY_SAMPLES_PASSED_CONSERVATIVE                0x8D6A
    :gl/SAMPLER_BINDING                                0x8919
    :gl/RGB10_A2UI                                     0x906F
    :gl/INT_2_10_10_10_REV                             0x8D9F
    :gl/TRANSFORM_FEEDBACK                             0x8E22
    :gl/TRANSFORM_FEEDBACK_PAUSED                      0x8E23
    :gl/TRANSFORM_FEEDBACK_ACTIVE                      0x8E24
    :gl/TRANSFORM_FEEDBACK_BINDING                     0x8E25
    :gl/TEXTURE_IMMUTABLE_FORMAT                       0x912F
    :gl/MAX_ELEMENT_INDEX                              0x8D6B
    :gl/TEXTURE_IMMUTABLE_LEVELS                       0x82DF
  })

#?(:clj
 (defmacro defall []
   `(do ~@(map (fn [[k v]] `(def ~(symbol (name k)) ~v)) (merge gl1-enums gl2-enums)))))

(defall)