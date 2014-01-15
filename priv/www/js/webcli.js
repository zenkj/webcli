$(document).ready(function() {

    var g_currentcsid = 1;
    var g_csids = [];

    function log(msg) {
        //alert(msg);
    }


    function request(type, url, data, done, fail) {
        var xhr = $.ajax({url: url,
            type: type,
            data: data,
            dataType: 'json',
            cache: false});
        if (done) xhr.done(done);
        if (fail) xhr.fail(fail);
    }

    function post(url, data, done, fail) {
        request('POST', url, data, done, fail);
    }

    function get(url, data, done, fail) {
        request('GET', url, data, done, fail);
    }

    function updateDisplay(info) {
        if (!info) return;

        if (info.msgs) {
            for (var i in info.msgs) {
                var msg = info.msgs[i];
                var csid = msg.csid;
                var csidstr = 'cs-' + csid;
                var type = msg.type;
                var data = msg.data;
                var logclass = type;

                logmsg(type, data, csid);
            }
        }
    }

    function clearInputBox() {
        $('.cli-cmd-input').val('');
        $('.cli-cmd-input').focus();
    }

    function clicmd(msg){
        post('/clicmd', {cmd: msg, csid: g_currentcsid},
                function(result) { updateDisplay(result); },
                function(xhr, err) {
                    log("send message failed, please relogin");
                    //activeAll(false);
                    redirect();
                });
    }

    function set_term_attr(n, attr) {
        switch (n) {
        case 0:
            attr.reset = true;
            break;
        case 1:
            attr.reverse = true;
            break;
        case 30:
            attr.foreground = "black";
            break;
        case 31:
            attr.foreground = "red";
            break;
        case 32:
            attr.foreground = "green";
            break;
        case 33:
            attr.foreground = "yellow";
            break;
        case 34:
            attr.foreground = "blue";
            break;
        case 35:
            attr.foreground = "magenta";
            break;
        case 36:
            attr.foreground = "cyan";
            break;
        case 37:
            attr.foreground = "white";
            break;
        case 40:
            attr.background = "black";
            break;
        case 41:
            attr.background = "red";
            break;
        case 42:
            attr.background = "green";
            break;
        case 43:
            attr.background = "yellow";
            break;
        case 44:
            attr.background = "blue";
            break;
        case 45:
            attr.background = "magenta";
            break;
        case 46:
            attr.background = "cyan";
            break;
        case 47:
            attr.background = "white";
            break;
        }
    }

    function terminal2html(msg) {
        var inspan = false;
        var output = "";
        // octal 033 is esc, 133 is [
        var pattern = /\033\133[0-9;]*m/;
        var otherPattern = /\033\133[0-9;?]*[a-ln-z]/g;
        var attr = {"background":"white", "foreground":"black", "bold":false, "reverse":false, "reset":false};
        msg = msg.replace(otherPattern, '');

        while(true) {
            var pos = msg.search(pattern);
            if (pos < 0) {
                output += msg;
                if (inspan) {
                    output += '</span>';
                    inspan = false;
                }
                break;
            } else { 
                if (pos > 0) {
                    output += msg.substring(0, pos);
                    msg = msg.substring(pos);
                }
                var controls = msg.match(pattern);
                var control = controls[0];
                msg = msg.substring(control.length);
                if (control.length == 3) {
                    // no attribute specified
                    continue;
                }
                control = control.substring(2, control.length-1);
                var attrs = control.split(';');
                attr.reset = false;
                for (var i=0; i<attrs.length; i++) {
                    var n = parseInt(attrs[i]);
                    set_term_attr(n, attr);
                }

                if (attr.reset) {
                    attr = {"background":"white", "foreground":"black", "bold":false, "reverse":false, "reset":false};
                    if (inspan) {
                        output += '</span>';
                        inspan = false;
                    }
                } else if (attr.reverse)  {
                    if (inspan) {
                        output += '</span>';
                        inspan = false;
                    }
                    var b = attr.bold? 'font-weight:bold;' : '';
                    output += '<span style="color:'+attr.background+';background-color:'+attr.foreground + ';'+b+'">';
                    inspan = true;
                } else {
                    if (inspan) {
                        output += '</span>';
                        inspan = false;
                    }
                    var b = attr.bold? 'font-weight:bold;' : '';
                    output += '<span style="color:'+attr.foreground+';background-color:'+attr.background + ';'+b+'">';
                    inspan = true;
                }
            }
        }
        return output;
    }

    function logmsg(type, msg, csid) {
        if (csid == undefined) {
            csid = g_currentcsid;
        }
        var t2h = terminal2html(msg);
        var str = '<pre class="' + type + '">' + t2h + '</pre>';
        var logdom = $('#cs-' +csid+' .cli-log');
        $(str).appendTo(logdom);
        var wrapper = $('#cs-' +csid+' .cli-log-wrapper');
        wrapper.scrollTop(logdom.height());
    }

    function stdin(msg){
        logmsg('stdin', msg);
        post('/stdin', {stdin: msg, csid: g_currentcsid},
                function(result) { updateDisplay(result); },
                function(xhr, err) {
                    log("send message failed, please relogin");
                    //activeAll(false);
                    redirect();
                });
    }
    function sendMsg() {
        var data = $('.cli-cmd-input').val();
        var trimed = $.trim(data);
        if (trimed && trimed.length > 1 && trimed.charAt(0) == ':') {
            var cmd = $.trim(trimed.substring(1));
            if (cmd == 'q') {
            } else if (cmd == 'sp') {
            } else if (cmd == 'vs') {
            }
        } else {
            data = data + '\n';
            stdin(data);
        }
        clearInputBox();
    }

    $('.cli-cmd-enter').click(function() {
        sendMsg();
    });

    $('.cli-cmd-input').keydown(function(e) {
        switch(e.which) {
        case 13: //enter
            sendMsg();
            break;
        case 27: //escape
            clearInputBox();
            break;
        }
    });

    function start_daemon() {
        function daemon() {
            get('/response', {},
                    function(data) {
                        updateDisplay(data);
                        if (data && data.result) {
                            daemon();
                        } else {
                            log("hello failed: " + data);
                            redirect();
                        }
                    },
                    function(xhr, err) {
                        log("hello failed: " + err);
                        redirect();
                    });
        }

        daemon();
    }

    function redirect() {
        $('body').empty();
        window.location.assign("/login");
    }

    function getclisessions() {
        get('/clisessions', [],
            function(data) {
                if (!data || !data.result)
                    return;
                if (data.csids.length == 0)
                    return;
                g_csids = data.csids;

                // only start daemon after obtaining cli session info
                start_daemon();
            },
            function(xhr, err) {
                log("get friend error: " + err);
                redirect();
            });
    }

    getclisessions();

    $(".cli-cmd-input").focus();

});
