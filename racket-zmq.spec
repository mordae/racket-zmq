%global pkgsdir %{_datadir}/racket/extra-pkgs
%global docsdir %{_docdir}/racket

%global collection zmq

Name:		racket-zmq
Version:	20140903
Release:	1%{?dist}
Summary:	minimal ZeroMQ bindings for Racket

Group:		Development/Libraries
License:	LGPLv3
URL:		http://github.com/mordae/racket-zmq
Source0:	%{name}-%{version}.tar.gz

BuildArch:	noarch

BuildRequires:	racket racket-packaging racket-misc1
Requires:	racket zeromq3

%description
Library with very limited ZeroMQ bindings for Racket, but integrating with
it's event and thread system to the fullest.

%prep
%setup -q

%build
%{_libexecdir}/racket-build %{collection}

%install
%{_libexecdir}/racket-install %{collection}

%post
raco pkg install --installation --no-setup --force --deps force \
                 --link %{pkgsdir}/%{collection}
raco setup --no-user --doc-index --only %{collection} >/dev/null

%preun
raco pkg remove --installation --no-setup --force %{collection} >/dev/null

%postun
raco setup --no-user --doc-index --tidy >/dev/null

%files
%{pkgsdir}/*
%doc %{docsdir}/*

%changelog
