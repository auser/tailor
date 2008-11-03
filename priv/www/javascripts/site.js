$(document).ready(function() {
	var url = "/log";
  
  $("#window").bind("message", function(event, message) {
      $(this).triggerHandler("append", [message]);
      $(this).triggerHandler("poll");

  }).bind("error", function(event, message) {
      $(this).triggerHandler("poll");
      
  }).bind("append", function(event, message) {
      $("ol", this).prepend([
          '<li>',
          message,
          "</li>"].join(""));
      this.scrollTop = $("ol", this).get(0).offsetHeight;

  }).bind("poll", function(event) {
      var self = this;
      $.ajax({
          url: url + "?t=" + new Date().getTime().toString(),
          dataType: "json",
          method: "get",
          success: function(data) {
              if("ok" in data) {
                  $(self).triggerHandler("message", [data.ok]);
              } else if("error" in data) {
                  $(self).triggerHandler("error", [data.error]);
              } else {
                  alert("epic fail!");
              }
          }
      });
  }).triggerHandler("poll");
});