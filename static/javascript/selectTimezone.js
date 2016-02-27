window.Croniker = {};

Croniker.geolocate = function(success) {
  var geolocationJSON = JSON.stringify({ considerIp: true });
  var geolocationURL = "https://www.googleapis.com/geolocation/v1/geolocate?key=" + window.GOOGLE_API_KEY;

  $.ajax({
    contentType: "application/json; charset=utf-8",
    data: geolocationJSON,
    success: success,
    method: "POST",
    url: geolocationURL,
  });
};

Croniker.findTimezone = function(data, success) {
  var lat = data.location.lat;
  var lng = data.location.lng;
  var epoch = Date.now() / 10;
  var timezoneURL = "https://maps.googleapis.com/maps/api/timezone/json?location="+lat+","+lng+"&timestamp="+epoch+"&key="+window.GOOGLE_API_KEY;

  $.get(timezoneURL, success);
};

Croniker.selectTimezone = function(){
  var timeZoneSuccess = function(timezoneData) {
    $(function(){
      var $select = $("select");
      var $spinning = $("#spinning");
      $select.find("option[selected]").prop("selected", "");
      $select.find("option:contains('"+timezoneData.timeZoneId+"')").prop("selected", "selected");
      $spinning.text("(It's probably already selected.)");
    })
  };

  Croniker.geolocate(function(geolocationData){
    Croniker.findTimezone(geolocationData, timeZoneSuccess);
  });
};

Croniker.selectTimezone();
