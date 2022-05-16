module.exports = {
    content: ["./static/*.{html, js}", './*.html'],
  theme: {
    extend: {},
  },
    plugins: [
        require('@tailwindcss/forms')
    ],
}
