export const afterRender = () => new Promise(resolve => requestAnimationFrame(() => resolve()));
