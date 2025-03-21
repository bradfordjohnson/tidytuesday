const uniqueImageLinks = [...new Set([...document.querySelectorAll('img')].map(img => img.src))];

const jsonData = uniqueImageLinks.map(link => `
{
    "@context": "https://schema.org/",
    "@type": "ImageObject",
    "contentUrl": "${link}",
    "license": "https://creativecommons.org/licenses/by/4.0/",
    "creditText": "Ford Johnson",
    "creator": {
        "@type": "Person",
        "name": "Ford Johnson"
    },
    "copyrightNotice": "Ford Johnson"
},`).join('\n');

const jsonLdScript = `<script type="application/ld+json">
[${jsonData}]
</script>`;

const textarea = document.createElement('textarea');
textarea.value = jsonLdScript;
document.body.appendChild(textarea);

textarea.style.position = 'fixed';
textarea.style.top = '0';
textarea.style.left = '0';
textarea.style.width = '100vw';
textarea.style.height = '100px';
textarea.style.fontSize = '16px';
textarea.style.zIndex = '-1';
textarea.style.opacity = '0';

textarea.select();
document.execCommand('copy');

document.body.removeChild(textarea);

console.log('Copied JSON-LD wrapped in <script> tags to clipboard!');
