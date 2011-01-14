<html>
    <head>
        <title>Kansas Lava Unit Tests</title>
        <style>
         <!--
            div { margin: 0em 2em 0.1em 1em;
                }
            #summary { white-space: pre-line;
                       margin-bottom: 2em;
                     }
            .additional { display: none; }
            .status { float: right; }

            .shallowfail { background-color: red; }
            .shallowpass { background-color: green; }
            .simgenerated { background-color: green; }
            .codegenfail { background-color: yellow; }
            .compilefail { background-color: orange; }
            .simfail { background-color: orange; }
            .comparefail { background-color: red; }
            .pass { background-color: green; }
         -->
        </style>
        <script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.min.js"></script>
        <script type="text/javascript">
            $(document).ready(function() {
                $("div.header").click(function() {
                    $(this).next(".additional").toggle();
                });
            });
        </script>
    </head>
    <body>
        <div id="summary">
