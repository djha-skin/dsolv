;;;; tests/apt.lisp
;;;;
;;;; Ported from degasolv/test/degasolv/pkgsys/apt_test.clj
;;;;
;;;; Tests for the APT package system: deb-to-degasolv-requirements
;;;; and apt-repo functions.

(defpackage #:com.djhaskin.dsolv/tests/apt
  (:use #:cl)
  (:import-from #:com.djhaskin.dsolv/resolver
    #:present
    #:make-version-predicate
    #:version-predicate
    #:vp-relation
    #:vp-version
    #:req-id
    #:req-spec
    #:req-status
    #:pi-id
    #:pi-version
    #:pi-location
    #:pi-requirements
    #:make-package-info)
  (:import-from #:com.djhaskin.dsolv/pkgsys/apt
    #:deb-to-degasolv-requirements
    #:apt-repo)
  (:import-from #:com.djhaskin.svers
    #:maven-vercmp)
  (:import-from #:fset)
  (:import-from #:parachute
    #:define-test
    #:true
    #:false
    #:is)
  (:local-nicknames
    (#:f #:fset)))

(in-package #:com.djhaskin.dsolv/tests/apt)

;;; ─── Tests: deb-to-degasolv-requirements ────────────────────────────────────

(define-test deb-to-degasolv-requirements-test
  :parent nil
  "Test the deb-to-degasolv-requirements function that converts
   Debian-style dependency strings to degasolv requirement format."
  (true (null (deb-to-degasolv-requirements nil)))
  (true (null (deb-to-degasolv-requirements "")))
  (let ((result (deb-to-degasolv-requirements "a (>>5.0), b (>= 4.0)")))
    (true (listp result))
    (is = 2 (length result))
    (let ((first-alt (first result)))
      (true (listp first-alt))
      (is = 1 (length first-alt))
      (let ((req (first first-alt)))
        (is string= "a" (req-id req))
        (true (listp (req-spec req)))
        (is = 1 (length (req-spec req)))
        (let ((disj (first (req-spec req))))
          (is = 1 (length disj))
          (let ((vp (first disj)))
            (is eql :greater-than (vp-relation vp))
            (is string= "5.0" (vp-version vp))))))
    (let ((second-alt (second result)))
      (true (listp second-alt))
      (is = 1 (length second-alt))
      (let ((req (first second-alt)))
        (is string= "b" (req-id req))
        (let ((spec (req-spec req)))
          (true (listp spec))
          (is = 1 (length spec))
          (let ((disj (first spec)))
            (is = 1 (length disj))
            (let ((vp (first disj)))
              (is eql :greater-equal (vp-relation vp))
              (is string= "4.0" (vp-version vp))))))))
  (let ((result (deb-to-degasolv-requirements "a|b (<< 1.2.3), c (= 1.0.0)")))
    (true (listp result))
    (is = 2 (length result))
    (let ((first-alt (first result)))
      (true (listp first-alt))
      (is = 2 (length first-alt)))
    (let ((second-alt (second result)))
      (true (listp second-alt))
      (is = 1 (length second-alt))
      (is string= "c" (req-id (first second-alt)))))
  (let ((result (deb-to-degasolv-requirements "a:any")))
    (true (listp result))
    (is = 1 (length result))
    (let ((alt (first result)))
      (is = 1 (length alt))
      (is string= "a" (req-id (first alt))))))

;;; ─── Tests: apt-repo ────────────────────────────────────────────────────────

(define-test test-apt-repo
  :parent nil
  "Test the apt-repo function that creates repository query functions
   from APT repository data."
  (let* ((apt-data "Package: foo:any
Priority: optional
Section: misc
Installed-Size: 27
Maintainer: Luke Yelavich <themuso@ubuntu.com>
Architecture: amd64
Version: 0.1.11-0ubuntu3
Depends: liba11y-profile-manager-0.1-0 (>= 0.1.11), libc6:any (>= 2.4), libglib2.0-0 (>= 2.26.0)
Filename: pool/main/f/foo/foo_0.1.11-0ubuntu3_amd64.deb
Size: 6310
MD5sum: 88048849b5897f17b987c0bfd8f1c899
SHA1: 3520ea78e489da35a7e71048dd5ff3fe6d99e13e
SHA256: a14a3bf010d5e5f8a2b46ff94836808cca02ebb1610b9e36558d3a4d8a7296d9
Description: Accessibility Profile Manager - Command-line utility
Multi-Arch: foreign
Homepage: https://launchpad.net/a11y-profile-manager
Description-md5: ecbac70f8ff00c7dbf5fdc46d7819613
Bugs: https://bugs.launchpad.net/ubuntu/+filebug
Origin: Ubuntu
Supported: 9m
Task: ubuntu-live, ubuntu-gnome-desktop, ubuntu-mate-live

Package: a11y-profile-manager
Priority: optional
Section: misc
Installed-Size: 27
Maintainer: Luke Yelavich <themuso@ubuntu.com>
Architecture: amd64
Version: 0.1.11-0ubuntu3
Depends: liba11y-profile-manager-0.1-0 (>= 0.1.11), libc6 (>= 2.4), libglib2.0-0 (>= 2.26.0)
Filename: pool/main/a/a11y-profile-manager/a11y-profile-manager_0.1.11-0ubuntu3_amd64.deb
Size: 6310
MD5sum: 88048849b5897f17b987c0bfd8f1c899
SHA1: 3520ea78e489da35a7e71048dd5ff3fe6d99e13e
SHA256: a14a3bf010d5e5f8a2b46ff94836808cca02ebb1610b9e36558d3a4d8a7296d9
Description: Accessibility Profile Manager - Command-line utility
Multi-Arch: foreign
Homepage: https://launchpad.net/a11y-profile-manager
Description-md5: ecbac70f8ff00c7dbf5fdc46d7819613
Bugs: https://bugs.launchpad.net/ubuntu/+filebug
Origin: Ubuntu
Supported: 9m
Task: ubuntu-live, ubuntu-gnome-desktop, ubuntu-mate-live

Package: a11y-profile-manager-doc
Priority: optional
Section: doc
Installed-Size: 118
Maintainer: Luke Yelavich <themuso@ubuntu.com>
Architecture: all
Source: a11y-profile-manager
Version: 0.1.11-0ubuntu3
Recommends: devhelp
Filename: pool/main/a/a11y-profile-manager/a11y-profile-manager-doc_0.1.11-0ubuntu3_all.deb
Size: 13362
MD5sum: d47968ecee4e0ef7b647b87022c9f6c7
SHA1: f14bf9a6cf95b7f0e22e03c9628ab8c394e32a1e
SHA256: 9827eea0cdb6f142057dc5768a8980f91f21dbb1544c9860a77e75ff3dfc183c
Description: Accessibility Profile Manager - Documentation
Homepage: https://launchpad.net/a11y-profile-manager
Description-md5: 1c71821ee46c31ca86e8242f7517c26e
Bugs: https://bugs.launchpad.net/ubuntu/+filebug
Origin: Ubuntu
Supported: 9m")
       (repo (apt-repo "http://us.archive.ubuntu.com/ubuntu/" apt-data)))
    (let ((foo-result (funcall repo "foo")))
      (true (not (f:empty? foo-result)))
      (is string= "foo" (pi-id (f:first foo-result)))
      (is string= "0.1.11-0ubuntu3" (pi-version (f:first foo-result))))
    (let ((apm-result (funcall repo "a11y-profile-manager")))
      (true (not (f:empty? apm-result)))
      (is string= "a11y-profile-manager" (pi-id (f:first apm-result)))
      (is string= "0.1.11-0ubuntu3" (pi-version (f:first apm-result))))
    (let ((apmd-result (funcall repo "a11y-profile-manager-doc")))
      (true (not (f:empty? apmd-result)))
      (is string= "a11y-profile-manager-doc" (pi-id (f:first apmd-result)))
      (is string= "0.1.11-0ubuntu3" (pi-version (f:first apmd-result))))))