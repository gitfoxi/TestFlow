
-- Testflow Syntax
--
-- Data representing the BNF

data TestFlow =
  TestFlow
    { langRev :: LangRev
    , sections :: Sections
    }

type Sections = [Section]

data Section =
  Section
    { information :: InformationSection
    , declarations :: DeclarationsSection
    , flags :: FlagsSection
    , tests :: TestsSection
    , testfunctions :: TestfunctionSection
    , datasets :: DatasetSection
    , testSuites :: TestSuitesSection
    , initialize :: InitializeSection
    , download :: DownloadSection
    , abort :: AbortSection
    , pause :: PauseSection
    , reset :: ResetSection
    , exit :: ExitSection
    , testFlow :: TestFlowSection
    , binning :: BinningSection
    , context :: SetupSection
    , hardwareBinDescriptions :: HardwareBinDescriptionsSection
    }

data LanguageRevision =
  LanguageRevision Int

data InformationSection =
  InformationSection
   { description :: Text
   , device_name :: Text
   , device_revision :: Text
   , test_revision :: Text
   }

data DeclarationsSection = [Declaration]

Declaration = HashMap.Map VarName Expr

data VarName =
    Label
  | FlagName Text
  | TestSuiteFlag TestSuite Text

-- TODO: * <test_suite>.ffc_on_fail}

data Flags = HashMap.Map VarName Flag

data FlagType = Flag | UserFlag

data Flag =
  Flag
   { flagType :: FlagType
   , flagName :: Text
   , flagValue :: Int
   }

-- Won't keep track of specific valid testflow flags because version compatibility


type Datasets = [DataSet]

data Dataset =
  Dataset
    { datasetLabel :: Text
    , dataSetFiles :: [Text]
    }
   
-- maps test name to optional user procedure
type Tests = HashMap.Map Text (Maybe Text)

data TestsSection
  TestSuite
   { userProcedure :: Maybe Text
   , testSpec :: Text
   } 
{-
<testfunction_section> := <testfunction_section> [<testfunction>]
<testfunction> := <testfunction_identifier>:
testfunction_description = <string>;
testfunction_parameters = <string>;
<testfunction_identifier> := <label>
<test_suites_section> := <test_suites_section> [<test_suite_entry>]
<test_suite_entry> := <test_suite>:
{datasets = <dataset_label>; |
tests = <test_label>; |
test_level = <test_level>; |
local_flags = <ts_flag> [, ...];|
* ffc_on_fail = <ffc_count>; |
override = <boolean>; | 
override_dpsset = <expression>; | 
override_levset = <expression>; | 
override_seqlbl = <expression>; | 
override_timset = <expression>; | 
* override_wvfset = <expression>; | 
override_testf = <label>; |
site_match = {0 | 1}; |
site_control = "<site_control_info>";}
<test_suite> := <label>
<test_level> := <integer>
* <ffc_count> := <integer>
<site_control_info>:= <sequence_type>: [<sequence_info>]
(<sequence_info> is omitted if 
<sequence_type> = serial or parallel)
<sequence_type> := {serial|parallel|semiparallel|other}
<sequence_info> := {<size>:<cycle>:|<sequence_order>}
(If <sequence_type> = semiparallel,
<size>:<cycle>: is used. 
If <sequence_type> = other,
<sequence_order> is used.)
<size> := an integer number (where 0 = undefined)
<cycle> := an integer number (where 0 = undefined),
or -1 for "unbouded"
<sequence_order> := <sequence_order_element>:
[<sequence_order_element>:]
<sequence_order_element> := an integer number 
(where, 0 = undefined, or 
a pogitive integer = test order)
<ts_flag> := {bypass |
set_pass |
set_fail |
hold |
hold_on_fail |
output_on_pass |
output_on_fail |
value_on_pass |
value_on_fail |
per_pin_on_pass |
per_pin_on_fail }
<initialize_section> := <test_suite_entry>
<download_section> := <test_suite_entry>
<abort_section> := <test_suite_entry>
<pause_section> := <test_suite_entry>
<reset_section> := <test_suite_entry>
<exit_section> := <test_suite_entry>
<test_flow_section> := <statement>;
[...]
<statement> := {<assignment> |
run(<test_suite>) |
run_and_branch(<test_suite>) 
then <statement> [else <statement>] |
print(<print_expression>) |
print_dl(<print_expression>) |
if <expression> then <statement>
[else <statement>] |
while <expression> do <statement> |
for <assignment>;<expression>;<assignment>; 
do <statement> |
repeat <statement>; until <expression> |
'{' <statement> [;<statement>]... '}'
[[<compound_state>], 
[<compound_label>],
[<compound_description>]] |
stop_bin(<major_bin>, <minor_bin>, , <good_flag>,
<reprobe_flag>, <color>, <physical_bin>
[, <over_on_flag>]);}
NOTEFor compatibility reasons to older versions, SmarTest saves stop_bin with a blank in between commas between <minor_bin> and <good_flag>.
<assignment> :={@<var_name> = <expression> |
@<var_name> = <string>}
<compound_state> := {open | closed}
<compound_label> := <string>
<compound_description> := <string>
<over_on_flag> := {over_on | not_over_on}
<good_flag> := {good | bad}
<reprobe_flag> := {reprobe | noreprobe}
<print_expression> := {<expression> |
<string_with_vars> }
<hardware_bin_descriptions_section> := {<integer> = <string> ;
[...]}
<binning_section> :=
otherwise bin = <major_bin> <minor_bin>, , <good_flag>, 
<reprobe_flag>, <color>, 
<physical_bin>[, <over_on_flag>] ;
NOTEFor compatibility reasons to older versions, SmarTest saves otherwise_bin_description with a blank in between commas between <minor_bin> and <good_flag>.
<major_bin> := <character>[<character>]
<minor_bin> := <string_with_vars>
<evaluation_limit> := <integer>
<consideration window> := <integer>
<action_limit> := <integer>
<message> := <string> given as report warning or 
stop message
<setup_section> := context_config_file = <string>;
context_levels_file = <string>;
context_timing_file = <string>;
context_vector_file = <string>;
context_attrib_file = <string>;
context_mixsgl_file = <string>;
<color> := an integer number (where 0 = black, 1 = white, 2 = red,
3 = yellow, 4 = green, 5 = cyan, 6 = blue, 7 = magenta), 
or the name of the color in lower case.
<expression> := an arithmetic, string and/or boolean
expression with operators:
+
-
*
/
>
>=
<
<=
==
!=
(
)
and
or
not
and operands:
pass(<test_suite>)
fail(<test_suite>)
tf_result(<test_suite>,<result_index>)
has_run(<test_suite>)
has_not_run(<test_suite>)
* wsus()
tsus()
lsus()
dsus()
bsus()
( expression )
@<var_name>
<real_number>
<string>
<string_with_vars> := "[[<chars>] \@<var_name> ]... [<chars>]"
<result_index> := <integer> in the range from 0 to 
TF_MAX_NR_RSLTS-1
<label> := [A-Za-z]<chars>
<string> := any sequence of ASCII characters delimited 
by ""
<chars> := any sequence of ASCII characters without "
<real_number> := any integer or real number, containing [0-9]
and for real numbers, the decimal point.
<integer> := an integer number
<boolean> := {0 | 1}
Example
hp93000,testflow,0.1
language_revision = 1;
information
device_name = "74145";
device_revision = "1";
test_revision = "1";
description = "The 74145 is a BCD-to-Decimal Converter";
end
--------------------------------------------------
declarations
end
--------------------------------------------------
flags
datalog_formatter = 0;
datalog_sample_size = 1;
graphic_result_displa = 1;
state_display = 0;
print_wafermap = 0;
ink_wafer = 0;
max_reprobes = 1;
temp_monitor = 1;
calib_age_monitor = 1;
diag_monitor = 1;
current_monitor = 1;
set_pass_level = 0;
set_fail_level = 0;
set_bypass_level = 0;
hold_on_fail = 0;
optimized_mode = 1;
global_hold = 0;
debug_mode = 0;
parallel_mode = 1;
global_overon = 0;
end
--------------------------------------------------
testfunctions
tf_1:
testfunction_description = "Continuity";
testfunction_parameters = "continuity;@;0;uA; 10;mV; 200;mV; 
    700;mV; 900;ms;1;0;Continuity ($P);";
tf_6:
testfunction_description = "Basic functional test";
testfunction_parameters = "functional";
end
--------------------------------------------------
test_suites
Continuity:
override = 1;
override_wvfset = 1;
override_timset = 1;
override_levset = 1;
override_dpsset = 1;
override_seqlbl = "DEFAULT";
override_testf = tf_1;
Basic_functional_tes:
override = 1;
override_wvfset = 1;
override_timset = 1;
override_levset = 1;
override_dpsset = 1;
override_seqlbl = "DEFAULT";
override_testf = tf_6;
end
--------------------------------------------------
test_flow
run_and_branch(Continuity) then
{
}
else
{
stop_bin "AA", "Continuity failed", , bad,noreprobe,red, ,over_on;
}
run_and_branch(Basic_functional_tes) then
{
}
else
{
stop_bin "BB", "Basic functional test failed", , bad, noreprobe,red, ,over_on;
}
stop_bin "OK", "Everything went fine", , good, noreprobe,
green, , over_on;
end
-------------------------------------------------
binning
otherwise bin = "db", "", bad,noreprobe,red, , not_over_on;
end
-------------------------------------------------
context
context_config_file = "74145";
context_levels_file = "LowPower";
context_timing_file = "Fast";
context_vector_file = "74145";
context_attrib_file = "";
context_mixsgl_file = "";
{- TODO: Check for backwards compatibility
 -
Topic was introduced before version 6.3.2
Change in SOC 7.1Testflow optimization function removed
Changed in SOC 7.2Removed report_to_file flag
Changed in SOC 7.2Removed report_to_printer flag
Changed in SOC 7.2Removed datalog_to_file flag
Changed in SOC 7.2Removed datalog_to_printer flag
Changed in SOC 7.2Removed datalog_to_report_win flag
Changed in SOC 7.2.2Removed ooc_rule, OOC_watch, and OOC_sample_size related syntaxes
-}
-}
