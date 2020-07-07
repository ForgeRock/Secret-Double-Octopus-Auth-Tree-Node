package org.octopus.octopusNode;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.security.PublicKey;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;

import javax.inject.Inject;

import com.google.inject.Singleton;

@Singleton
public class OctopusCertServiceImpl implements OctopusCertService {

    private PublicKey publicKey;

    @Inject
    public OctopusCertServiceImpl() {
    }

    @Override
    public PublicKey getPublicKey(String certStr) throws CertificateException {
        if (publicKey == null) {
            StringBuilder builder = new StringBuilder(certStr);
            int first = builder.indexOf(" ");
            int last = builder.lastIndexOf(" ");
            builder.setCharAt(first, '#');
            builder.setCharAt(last, '#');
            String formatted = builder.toString();
            formatted = formatted.replaceAll(" ", "\n");
            formatted = formatted.replaceAll("#", " ");
            CertificateFactory cf = CertificateFactory.getInstance("X.509");
            InputStream targetStream = new ByteArrayInputStream(formatted.getBytes());
            Certificate cert = cf.generateCertificate(targetStream);
            publicKey = cert.getPublicKey();
        }
        return publicKey;
    }
}
