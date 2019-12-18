const secrets = require('./secrets.js')

module.exports = function(grunt) {

    let branch = grunt.option('branch') || 'default'

    grunt.loadNpmTasks('grunt-screeps');

    grunt.initConfig({
        screeps: {
            options: {
                email: secrets.email,
                password: secrets.password,
                branch: branch,
                ptr: false
            },
            dist: {
                src: ['output/main.js']
            }
        }
    });
}