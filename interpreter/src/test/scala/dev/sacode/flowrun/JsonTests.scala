package dev.sacode.flowrun

import utest.*
import ba.sake.tupson.*
import dev.sacode.flowrun.ast.Program
import dev.sacode.flowrun.ast.Statement
import dev.sacode.flowrun.ast.Expression

object JsonTests extends TestSuite {

  val tests = Tests {
    /* v1 tests */
    test("parse v1 JSON #1") {
      val json = """
        {"id":"id_bdae8193_a6cc_410c_8081_9e8e48de4baf","name":"BMI calculator","config":{"lang":"java","showFunctions":true,"showGenCode":false},"main":{"rawId":"main","name":"main","parameters":[],"tpe":"Void","statements":[{"id":"id_41b3ad45_658d_4d9a_aca5_81b0d5244ebd","@type":"Begin"},{"id":"id_fe750971_f73f_44e0_a97c_8ce7d369a808","name":"w","tpe":"Real","initValue":null,"@type":"Declare"},{"id":"id_bb0689c5_6aac_4ced_b118_36a4aba07adc","name":"h","tpe":"Real","initValue":null,"@type":"Declare"},{"id":"id_1c46aa52_01ef_48f4_b8d7_0345ea75031c","name":"w","prompt":"Please enter your weight in kg: ","@type":"Input"},{"id":"id_72999a30_2972_4490_a6aa_22a134fbf48d","name":"h","prompt":"Please enter your height in m: ","@type":"Input"},{"id":"id_7586ac24_0421_45c0_9578_05e216a072ec","name":"bmi","tpe":"Real","initValue":"bmi(h,w)","@type":"Declare"},{"id":"id_e405a479_4e43_4980_bc5f_63294792eec9","value":"\"Your BMI is \" + bmi","newline":true,"@type":"Output"},{"id":"id_5f9cfb07_6f24_4b4e_89e6_45c2fa8b6bb0","value":"printType(bmi)","@type":"Call"},{"id":"id_b493fab8_3bb3_4c63_85f7_331095562863","maybeValue":null,"@type":"Return"}]},"functions":[{"rawId":"id_92fac955_5ee5_4d5a_86c0_965cb72983fc","name":"bmi","parameters":[{"id":"7dfcbb2d-a408-47bd-bd4f-afa837b0e702","name":"height","tpe":"Real"},{"id":"644569fd-b36b-4818-8e75-ab25784a3a77","name":"weight","tpe":"Real"}],"tpe":"Real","statements":[{"id":"id_98959eed_7adc_45c0_8c59_f15b79b7e88b","@type":"Begin"},{"id":"id_767f6fb3_8455_4388_b878_087a40036cab","maybeValue":"weight / (height*height)","@type":"Return"}]},{"rawId":"id_d4da7911_bdd4_4d61_a95c_8787bd8e2e9c","name":"printType","parameters":[{"id":"46dd8e46-3202-4814-8f5f-64aaa3e19b88","name":"bmi","tpe":"Real"}],"tpe":"Void","statements":[{"id":"id_1d58d7ac_0fd7_4992_960d_347904ec843f","@type":"Begin"},{"id":"id_ac4e503c_0b8c_40bb_a2fc_667bcac69b11","condition":"bmi<18.5","trueBlock":{"id":"id_f601a9f0_f810_428c_a75a_555216bca1ae","statements":[{"id":"id_bfd6d514_7c5d_4331_8795_1ef3e395d8c3","value":"\"underweight\"","newline":true,"@type":"Output"}]},"falseBlock":{"id":"id_ccac4547_6f97_4d73_b92a_6902090fdf13","statements":[{"id":"id_21191261_3ac8_4a49_9345_0d3d0196fef4","name":"x","tpe":"Integer","initValue":null,"@type":"Declare"},{"id":"id_7752640a_53f9_4ea5_bd07_fc9a5a2d394e","name":"x","prompt":null,"@type":"Input"},{"id":"id_e7145259_06e6_4913_8b4d_6e94dc543dae","condition":"bmi<24.9","trueBlock":{"id":"id_afea93d2_9eb5_4fa8_a8d8_7aaf6f1ae43d","statements":[{"id":"id_7acf1e2c_bf66_4484_8cb8_817fc7af12e1","value":"\"normal\"","newline":true,"@type":"Output"}]},"falseBlock":{"id":"id_cf0582eb_ef64_4cb7_98e5_72f4709ccbc5","statements":[{"id":"id_d4f05a29_dcca_42e8_867c_1540013c43df","value":"\"overweight\"","newline":true,"@type":"Output"}]},"@type":"If"}]},"@type":"If"},{"id":"id_95656e91_9c5b_4363_9312_fbba74fdf63e","maybeValue":null,"@type":"Return"}]}],"version":"0.1"}
      """

      val program = json.parseJson[Program]
      assert(program.id == "id_bdae8193_a6cc_410c_8081_9e8e48de4baf")
      assert(program.name == "BMI calculator")
      assert(program.config.showDebugVars == true) // new property added
    }

    test("parse v1 JSON #2") {
      val json = """
        {"id":"id_67d52475_13e6_4290_80e7_5294370f5e3f","name":"leggi due numeri positivi","config":{"lang":"c++","showFunctions":true,"showGenCode":true},"main":{"rawId":"main","name":"main","parameters":[],"tpe":"Void","statements":[{"id":"id_51d0378d_6b52_4105_8c34_69fcd8a26d60","@type":"Begin"},{"id":"id_b0051a9d_932a_43ad_b5ea_b32258e29ad2","name":"n","tpe":"Integer","initValue":null,"@type":"Declare"},{"id":"id_119259d8_84f2_43cc_b21c_246162a1ad10","name":"k","tpe":"Integer","initValue":null,"@type":"Declare"},{"id":"id_f5501dd2_078a_4067_9761_c3f510c2f06d","name":"divisione","tpe":"Integer","initValue":null,"@type":"Declare"},{"id":"id_0d6205a9_8b7e_4b89_ad05_febf84fa9d0b","name":"n","prompt":null,"@type":"Input"},{"id":"id_3038c25f_0516_4c79_8682_236ea901fe38","name":"k","prompt":null,"@type":"Input"},{"id":"id_7e1353fa_27fa_4fda_8aa4_897508161430","condition":"n>k","trueBlock":{"id":"id_2d53dc45_ecdd_4d1c_9336_7be7572b06da","statements":[{"id":"id_12365241_7a0a_42c9_9e68_4a8a117ac7e2","value":"n","newline":true,"@type":"Output"},{"id":"id_9481ca3c_e6bf_4a78_b64f_b1384ea02d69","value":"k","newline":true,"@type":"Output"}]},"falseBlock":{"id":"id_d63c662a_a8b1_44a2_99eb_0757a063b4c9","statements":[{"id":"id_3d883af6_d657_469d_ba89_cff24041c499","value":"k","newline":true,"@type":"Output"},{"id":"id_a637484b_1276_4775_920a_711747b655fd","value":"n","newline":true,"@type":"Output"}]},"@type":"If"},{"id":"id_ad6cdee9_d659_4bf3_9771_985517298f4c","condition":"n%k==0","trueBlock":{"id":"id_b62c326d_b9b5_4edb_8298_96dd0f114083","statements":[{"id":"id_94b490fb_8ee6_4ab3_a8db_7f030efbe781","value":"\"n è divisibile per k\"","newline":true,"@type":"Output"},{"id":"id_122db0b4_9927_475a_9339_70b2522a9bbe","name":"divisione","value":"n/k","@type":"Assign"},{"id":"id_1ca0c894_2e76_499f_8169_4ccfdcc847a1","value":"divisione","newline":true,"@type":"Output"},{"id":"id_498fe4e4_914d_4981_9114_4a967ae3b505","value":"\"i multipli di n sono:\"","newline":true,"@type":"Output"},{"id":"id_d0bd5bd9_5451_4a02_b92a_1743beeb37b7","value":"divisione","newline":true,"@type":"Output"},{"id":"id_ff71ec5a_be69_48f7_a192_6a04b9b5ab5d","value":"k","newline":true,"@type":"Output"}]},"falseBlock":{"id":"id_27bf0659_de04_4dc6_be65_49e3de4c8287","statements":[{"id":"id_457adc84_1782_4188_98e2_4e2b94f48c73","value":"\"n non è divisibile per k\"","newline":true,"@type":"Output"}]},"@type":"If"},{"id":"id_b120dcc8_3543_4188_b445_35e19d4942d4","name":"divisore","tpe":"Integer","initValue":"2","@type":"Declare"},{"id":"id_01a627a3_4be3_412c_8a31_9328e38f5a22","name":"vero","tpe":"Boolean","initValue":null,"@type":"Declare"},{"id":"id_8f4b4bdc_016c_4e46_9e36_e94503d6ebb3","name":"primo","tpe":"Boolean","initValue":null,"@type":"Declare"},{"id":"id_bd918269_6cc7_48d4_bc8f_f5691b5a1692","name":"primo","value":"vero","@type":"Assign"},{"id":"id_374d096e_10de_4475_9293_b56fbcce3cca","condition":"n>0","body":{"id":"id_2a9cf384_4cd6_4389_8978_3b06d60c5cd5","statements":[{"id":"id_f3aad108_6895_459c_96d4_ee171b05a164","condition":"divisore<n","trueBlock":{"id":"id_9c89e5f0_3388_4347_bd87_12871c0e6562","statements":[{"id":"id_87246cd9_c8f2_4839_bf39_6818084aeb49","condition":"n%divisore","trueBlock":{"id":"id_e0640745_b1aa_47e2_b099_648bc160ef1e","statements":[{"id":"id_4185a64d_fb54_460b_832f_7919c0fe3d33","name":"primo","value":"falso","@type":"Assign"}]},"falseBlock":{"id":"id_837d82a0_6654_4968_a29e_e81f3a870458","statements":[]},"@type":"If"},{"id":"id_87b784e0_7af9_423c_98f1_3599118583bd","name":"divisore","value":"divisore+1","@type":"Assign"}]},"falseBlock":{"id":"id_c3ff860f_b955_40ad_8fd0_e865f9f19bec","statements":[{"id":"id_f686749e_d0e8_4add_a1f4_ece330756f4f","condition":"primo","trueBlock":{"id":"id_66e770b2_f1a4_4b66_9231_e402d97b4028","statements":[{"id":"id_c2941f06_14cd_48f3_90e1_9b2f93fa215c","value":"\"il numero è primo\"","newline":true,"@type":"Output"}]},"falseBlock":{"id":"id_3ffc1e9f_fdec_4042_bde2_c07c1bbf997a","statements":[{"id":"id_127296f0_8cef_4123_a839_d06caf2381b6","value":"\"il numero non è primo\"","newline":true,"@type":"Output"}]},"@type":"If"},{"id":"id_da1d7fd7_3294_4478_b8ca_ed2ccbfc13dd","name":"n","value":"0","@type":"Assign"}]},"@type":"If"}]},"@type":"While"},{"id":"id_cec51944_464a_4ffb_822f_ef076d68a264","value":"\"output\"","newline":true,"@type":"Output"},{"id":"id_28df7e8e_0f3f_4b02_bc87_5cb544a1891e","maybeValue":null,"@type":"Return"}]},"functions":[],"version":"0.1"}
      """

      val program = json.parseJson[Program]
      assert(program.id == "id_67d52475_13e6_4290_80e7_5294370f5e3f")
      assert(program.config.showDebugVars == true) // new property added
    }

    test("parse v1 JSON statement #2") {
      val json = """
    {"id":"id_bb0689c5_6aac_4ced_b118_36a4aba07adc","name":"h","tpe":"Real","initValue":null,"@type":"Declare"}
      """

      val stmt = json.parseJson[Statement].asInstanceOf[Statement.Declare]

      assert(stmt.id == "id_bb0689c5_6aac_4ced_b118_36a4aba07adc")
      assert(stmt.name == "h")
      assert(stmt.tpe == Expression.Type.Real)
      assert(stmt.initValue == None)
    }

  }
}
