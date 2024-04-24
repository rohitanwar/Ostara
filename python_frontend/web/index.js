document.getElementById('file').addEventListener('change', function() {
    let file = this.files[0];
    let reader = new FileReader();
    reader.onload = function(e) {
        let contents = e.target.result;
        document.getElementById('output').textContent = contents;
        eel.save_data(contents); 
    };
    reader.readAsText(file);
})

