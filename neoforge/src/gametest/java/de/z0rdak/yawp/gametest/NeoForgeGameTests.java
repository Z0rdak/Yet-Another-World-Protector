package de.z0rdak.yawp.gametest;

import net.minecraft.core.registries.Registries;
import net.minecraft.gametest.framework.FunctionGameTestInstance;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.gametest.framework.TestData;
import net.minecraft.gametest.framework.TestEnvironmentDefinition;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.EventBusSubscriber;
import net.neoforged.neoforge.event.RegisterGameTestsEvent;
import net.neoforged.neoforge.registries.RegisterEvent;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.List;

/**
 * NeoForge registration of the shared scenarios, see {@link BlockInteractionScenarios}.
 * Every public static method taking a {@link GameTestHelper} is registered as a test function and test instance.
 */
@EventBusSubscriber(modid = NeoForgeGameTests.MOD_ID)
public final class NeoForgeGameTests {

    static final String MOD_ID = "yawp";
    private static final Identifier EMPTY_STRUCTURE = Identifier.fromNamespaceAndPath(MOD_ID, "empty");
    private static final int MAX_TICKS = 100;

    private NeoForgeGameTests() {
    }

    private static List<Method> scenarios() {
        return Arrays.stream(BlockInteractionScenarios.class.getDeclaredMethods())
                .filter(m -> Modifier.isPublic(m.getModifiers()) && Modifier.isStatic(m.getModifiers()))
                .filter(m -> m.getParameterCount() == 1 && m.getParameterTypes()[0] == GameTestHelper.class)
                .toList();
    }

    /**
     * Identifier of a scenario: the camelCase method name as snake_case.
     */
    private static Identifier idOf(Method method) {
        String snake = method.getName().replaceAll("([A-Z])", "_$1").toLowerCase();
        return Identifier.fromNamespaceAndPath(MOD_ID, snake);
    }

    @SubscribeEvent
    public static void registerFunctions(RegisterEvent event) {
        event.register(Registries.TEST_FUNCTION, registry -> scenarios().forEach(method ->
                registry.register(idOf(method), helper -> {
                    try {
                        method.invoke(null, helper);
                    } catch (InvocationTargetException e) {
                        if (e.getCause() instanceof RuntimeException re) throw re;
                        throw new RuntimeException(e.getCause());
                    } catch (IllegalAccessException e) {
                        throw new RuntimeException(e);
                    }
                })));
    }

    @SubscribeEvent
    public static void registerTests(RegisterGameTestsEvent event) {
        var environment = event.registerEnvironment(Identifier.fromNamespaceAndPath(MOD_ID, "default"), new TestEnvironmentDefinition.AllOf());
        scenarios().forEach(method -> {
            Identifier id = idOf(method);
            event.registerTest(id, new FunctionGameTestInstance(
                    ResourceKey.create(Registries.TEST_FUNCTION, id),
                    new TestData<>(environment, EMPTY_STRUCTURE, MAX_TICKS, 0, true)));
        });
    }
}
