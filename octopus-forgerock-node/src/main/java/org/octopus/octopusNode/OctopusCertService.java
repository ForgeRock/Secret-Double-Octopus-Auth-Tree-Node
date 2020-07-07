package org.octopus.octopusNode;

import java.security.PublicKey;
import java.security.cert.CertificateException;

public interface OctopusCertService {
    PublicKey getPublicKey(String cert) throws CertificateException;
}
