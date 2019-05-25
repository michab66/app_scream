/*
 * Copyright 1999-2004 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.commons.javaflow.stores;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import org.apache.bcel.Repository;
import org.apache.bcel.util.ClassLoaderRepository;
import org.apache.commons.javaflow.bytecode.transformation.ResourceTransformer;
import org.apache.commons.javaflow.bytecode.transformation.bcel.BcelClassTransformer;
import org.apache.commons.jci.stores.MemoryResourceStore;
import org.apache.commons.jci.stores.ResourceStore;
import org.apache.commons.jci.stores.TransactionalResourceStore;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * Due to the BCEL dependency handling all
 * classes have to be store first and then
 * be rewritten. Otherwise we could just
 * delegate to the TransformingResourceStore
 * 
 * @author tcurdt
 */
public final class JavaflowResourceStore extends TransactionalResourceStore {

    private final static Log log = LogFactory.getLog(JavaflowResourceStore.class);

    private final TransformingResourceStore tstore;
    private final Collection changes = new ArrayList();
    
    public JavaflowResourceStore() {
        this(new MemoryResourceStore());
    }
    
    public JavaflowResourceStore(final ResourceStore pStore) {
        super(pStore);
        tstore = new TransformingResourceStore(
                pStore,
                new ResourceTransformer[] { new BcelClassTransformer() }
                );
        Repository.setRepository(new ClassLoaderRepository(this.getClass().getClassLoader()));
    }

    public void write(final String pResourceName, final byte[] pResourceData) {
        super.write(pResourceName, pResourceData);
        changes.add(pResourceName);
    }

    public void onStart() {
        changes.clear();
    }

    public void onStop() {
        if (changes.size() > 0) {
            log.debug("rewriting"  + changes);
            
            for (Iterator it = changes.iterator(); it.hasNext();) {
                final String clazzName = (String) it.next();
                try {
                    final byte[] oldClazz = super.read(clazzName);
                    
                    if (oldClazz == null) {
                        throw new ClassNotFoundException("could not find " + clazzName);
                    }
                    
                    tstore.write(clazzName, oldClazz);
                    
                    log.debug("rewrote " + clazzName);
                } catch (ClassNotFoundException e) {
                    log.error("", e);
                }
            }
            
            changes.clear();
        }
    }
}
