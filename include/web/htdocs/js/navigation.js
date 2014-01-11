(function(){

    var SEARCH_URL = "/imgsearch/httpd_search:search";

    function AjaxShowImages(data){
        $('form').fadeOut();
        $('.results').fadeIn();

        // Populate results
        for (var i = 0;  i < data.length; i++){
            var url = data[i];
            $('.results').html(function(_index, old){
                return (old +
                        '<div class="result">' +
                        '<a href="' + url + '">' +
                        '<img width="100" height="100" src="' + url +'" />'+
                        '<strong>' + url + '</strong>' +
                        '</a></div><br/>');
            });
        }
    }

    function ReturnToMain(event){
        $('.results').fadeOut();
        $('form').fadeIn();
    }

    function initialize(){
        $('form').ajaxForm({
            success: AjaxShowImages,
            dataType: "json"
        });
        $('.navbar-brand').click(ReturnToMain);
    }

    $(document).ready(initialize);
})();
