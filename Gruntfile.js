const secrets = require('./secrets.js')

module.exports = function(grunt) {

    grunt.loadNpmTasks('grunt-screeps');

    grunt.initConfig({
        screeps: {
            options: {
                email: secrets.email,
                password: secrets.password,
                branch: 'default',
                ptr: false
            },
            dist: {
                src: ['output/main.js']
            }
        }
    });
}