<html>
    <head>
        <title>Kansas Lava Unit Tests</title>
        <style>
         <!--
            div { margin: 0em 2em 0.1em 1em;
                }
            #summary { margin-bottom: 2em; }
            #summary td { text-align: right; padding-right: 1em; }
            .huge { margin: 0em; font-size: 4em; }
            .kindahuge { margin: 0em; font-size: 2em; }
            .allpass { color: #99FF66; }
            .somepass { color: #FF8040; }
            .allfail { color: red; }
            .additional { display: none; }
            .additional > div { white-space: pre-line; }
            .status { float: right; }

            .shallowfail { background-color: #FF3366; }
            .shallowpass { background-color: #99FF66; }
            .simgenerated { background-color: #99FF66; }
            .codegenfail { background-color: #FFFF99; }
            .compilefail { background-color: #FFFF99; }
            .simfail { background-color: #FFFF99; }
            .comparefail { background-color: #FF3366; }
            .pass { background-color: #99FF66; }
         -->
        </style>
        <script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js"></script>
        <script type="text/javascript">
            $(document).ready(function() {
                $("div.header").click(function() {
                    $(this).next(".additional").toggle();
                });
                $("#cgf").click(function() {
                    $(".header").hide();
                    $(".additional").hide();
                    $(".codegenfail").show();
                });
                $("#vcf").click(function() {
                    $(".header").hide();
                    $(".additional").hide();
                    $(".compilefail").show();
                });
                $("#cpf").click(function() {
                    $(".header").hide();
                    $(".additional").hide();
                    $(".comparefail").show();
                });
                $("#osf").click(function() {
                    $(".header").hide();
                    $(".additional").hide();
                    $(".simfail").show();
                });
                $("#showall").click(function() {
                    $(".additional").hide();
                    $(".header").show();
                });
            });
        </script>
    </head>
    <body>
        <div id="summary">
