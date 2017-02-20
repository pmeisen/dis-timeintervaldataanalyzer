package net.meisen.dissertation.impl.dataintegration;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.dataintegration.IPreProcessor;
import net.meisen.dissertation.model.dataintegration.IPreProcessorConfig;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import javax.script.SimpleScriptContext;

/**
 * A script-based pre-processor.
 *
 * @author pmeisen
 */
public class ScriptPreProcessor implements IPreProcessor {

    /**
     * The {@code ExceptionRegistry} used to throw exceptions.
     */
    @Autowired
    @Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
    private IExceptionRegistry exceptionRegistry;

    private String script = null;
    private ScriptEngine engine = null;

    @Override
    public void setConfig(final IPreProcessorConfig config) {

        final ScriptPreProcessorConfig spp;
        if (config instanceof ScriptPreProcessorConfig) {
            spp = (ScriptPreProcessorConfig) config;
        } else {
            exceptionRegistry.throwRuntimeException(
                    ScriptPreProcessorException.class, 1000,
                    config == null ? null : config.getClass().getSimpleName());
            return;
        }

        // create a script engine manager
        final ScriptEngineManager factory = new ScriptEngineManager();

        // add some general bindings to the factory
        factory.put("config", config);

        this.engine = factory.getEngineByName(spp.getLanguage());
        if (this.engine == null) {
            exceptionRegistry.throwRuntimeException(ScriptPreProcessorException.class, 1003, spp.getLanguage());
        }
        this.script = spp.getScript();
    }

    @Override
    public IDataRecord process(final IDataRecord raw) {
        final ScriptContext recordCtx = new SimpleScriptContext();
        recordCtx.setAttribute("raw", raw, ScriptContext.ENGINE_SCOPE);
        recordCtx.setAttribute("result", new PreProcessedDataRecord(raw), ScriptContext.ENGINE_SCOPE);

        Object res;
        try {
            res = engine.eval(this.script, recordCtx);

            // check if we have an result, otherwise use the tmpRes as fallback
            if (res == null) {
                res = recordCtx.getAttribute("result");
            }
        } catch (final ScriptException e) {
            exceptionRegistry.throwRuntimeException(
                    ScriptPreProcessorException.class, 1001, e, e.getMessage(),
                    this.script);
            return null;
        }

        // validate the result
        if (res instanceof IDataRecord) {
            return (IDataRecord) res;
        } else {
            exceptionRegistry.throwRuntimeException(
                    ScriptPreProcessorException.class, 1002, res == null ? null
                            : res.getClass().getName(), res);
            return null;
        }
    }

    /**
     * Sets the {@code ExceptionRegistry} to be used. The registry is auto-wired
     * by the instance, nevertheless if auto-wiring is not possible, the method
     * should be called.
     *
     * @param exceptionRegistry the {@code ExceptionRegistry} to be used
     */
    public void setExceptionRegistry(final IExceptionRegistry exceptionRegistry) {
        this.exceptionRegistry = exceptionRegistry;
    }
}
