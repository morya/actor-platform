package im.actor.core.js.providers.webrtc;

import im.actor.core.js.modules.JsScheduller;
import im.actor.runtime.promise.Promise;
import im.actor.runtime.promise.PromiseFunc;
import im.actor.runtime.promise.PromiseResolver;

public class JsStreaming {

    public static Promise<JsMediaStream> getUserAudio() {
        return getUserMedia(JsUserMediaConstraint.audioOnly());
    }

    public static Promise<JsMediaStream> getUserVideo() {
        return getUserMedia(JsUserMediaConstraint.videoOnly());
    }

    public static Promise<JsMediaStream> getUserAudioVideo() {
        return getUserMedia(JsUserMediaConstraint.audioVideo());
    }

    public static Promise<JsMediaStream> getUserMedia(final JsUserMediaConstraint constraint) {
        return new Promise<>(new PromiseFunc<JsMediaStream>() {
            @Override
            public void exec(final PromiseResolver<JsMediaStream> resolver) {
                getUserMediaJs(constraint, new JsMediaCallback() {
                    @Override
                    public void onCreated(JsMediaStream mediaStream) {
                        resolver.result(mediaStream);
                    }

                    @Override
                    public void onError(JsUserMediaError error) {
                        resolver.error(new MediaException(error));
                    }
                });
            }
        });
    }

    protected static native void getUserMediaJs(JsUserMediaConstraint constraint, JsMediaCallback callback)/*-{
        $wnd.navigator.getUserMedia(constraint, function(stream) {
            callback.@im.actor.core.js.providers.webrtc.JsMediaCallback::onCreated(*)(stream);
        }, function(error) {
            callback.@im.actor.core.js.providers.webrtc.JsMediaCallback::onError(*)(error);
        });
    }-*/;
}
