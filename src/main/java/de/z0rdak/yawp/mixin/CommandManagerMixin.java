package de.z0rdak.yawp.mixin;

import com.mojang.brigadier.ParseResults;
import de.z0rdak.yawp.handler.CommandInterceptor;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(CommandManager.class)
public abstract class CommandManagerMixin {

    @Inject(method = "execute", at = @At(value = "HEAD"), cancellable = true, remap = false)
    public void execute(ParseResults<ServerCommandSource> parseResults, String command, CallbackInfoReturnable<Integer> cir) {
        int result = CommandInterceptor.handleModCommands(parseResults, command);
        if (result != 0) {
            cir.setReturnValue(1);
        }
    }
}