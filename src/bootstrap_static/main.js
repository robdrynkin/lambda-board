var TOKEN_KEY = "token-"

function onThreadsPageLoad() {
    $("#form-create-thread").submit(function(eventObj) {
        var token = generateToken();
        var name = $("#threadName").val()
        setCookie(TOKEN_KEY+name, token, null)
        $("<input/>").attr("type", "hidden")
          .attr("name", "token")
          .attr("value", token)
          .appendTo("#form-create-thread");
        return true;
    });
}

function onCommentsPageLoad() {
    var threadName = $("#threadName").text()
    var token = getCookie(TOKEN_KEY+threadName);
    if (token) {
        $(".comment-id").each(function() {
            var e = $(this);
            e.after(
                "<form method=\"post\" action=\"/delete_comment\">" +
                "<input type=\"hidden\" name=\"threadName\" value=\"" + threadName + "\"/>" +
                "<input type=\"hidden\" name=\"token\" value=\"" + token + "\"/>" +
                "<input type=\"hidden\" name=\"commentId\" value=\"" + e.text() + "\"/>" +
                "<button action=\"post\" type=\"submit\">Delete</button>" +
                "</form>"
           );
        })
    }
    $("#form-delete-comment").submit(function(eventObj) {
        var token = generateToken();
        $("<input/>").attr("type", "hidden")
          .attr("name", "token")
          .attr("value", token)
          .appendTo("#form-delete-comment");
        return true;
    });
}

function setCookie(name, value, days) {
    var expires = "";
    if (days) {
        var date = new Date();
        date.setTime(date.getTime() + (days*24*60*60*1000));
        expires = "; expires=" + date.toUTCString();
    }
    document.cookie = name + "=" + (value || "")  + expires + "; path=/";
}

function getCookie(name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length;i++) {
        var c = ca[i];
        while (c.charAt(0)==' ') c = c.substring(1,c.length);
        if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
    }
    return null;
}

function eraseCookie(name) {
    document.cookie = name +'=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;';
}

function generateToken() {
    var result = '';
    var characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
    var charactersLength = characters.length;
    var length = 32;
    for (var i = 0; i < length; i++) {
        result += characters.charAt(Math.floor(Math.random() * charactersLength));
    }
    return result;
}
