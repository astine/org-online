$(function () {
    $('.foldable').hide();
    $('.folder').click(function () {
	$('#body-' + this.id.substr(5)).toggle();
    });
  });
