(() => {
    const autoIndent = event => {
        event.preventDefault()
        
    }
    const isCodearea = node => node.type == 'textarea' && node.className == 'code'
    const handleTab = node => node.addEventListener('keydown', event => event.keyCode == 9 && autoIndent(event), false)
    const handleTabOfCodearea = node => isCodearea(node) && handleTab(node)
    const handleTabOfTree = node => handleTabOfCodearea(node) || node.childNodes.forEach(handleTabOfTree)
    new MutationObserver(mutations => mutations.forEach(mutation => mutation.addedNodes.forEach(handleTabOfTree)))
        .observe(document.querySelector('html'), { childList: true, subtree: true })
})()